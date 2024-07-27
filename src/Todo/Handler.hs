{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Todo.Handler where

import Control.Lens (at, use)
import Control.Lens.Setter ((%=), (.=))
import Control.Monad.State
import Data.Dependent.Map qualified as DMap
import Data.Foldable (for_)
import Data.IFunctor (At (..), returnAt)
import Data.IFunctor qualified as I
import Data.Singletons
import Todo.Common
import Todo.Type
import TypedFsm

type AllSt = AllState Todo TodoList (ActionStMap Todo)

type Op' to from = Op Todo AllSt IO () to from

putSt :: Sing (st :: Todo) -> (ActionVal Todo st) -> StateT AllSt IO ()
putSt st sval =
  allOtherState %= DMap.alter (const (Just sval)) st

actionHandler'
  :: forall action to
   . (SingI to, SingI action)
  => Op Todo AllSt IO (Maybe (ActionOutput action)) to (Action action to)
actionHandler' =
  getInput I.>>= \case
    SureAction val ->
      getInput I.>>= \case
        Yes -> returnAt (Just val)
        No -> I.do
          liftm $ putSt @action sing (ActionVal $ Right val)
          actionHandler'
    SubAction val -> returnAt (Just val)
    ExitAction -> returnAt Nothing

actionHandler
  :: forall action to
   . (SingI to, SingI action)
  => ActionInput action
  -> Op Todo AllSt IO (Maybe (ActionOutput action)) to (Action action to)
actionHandler input = I.do
  liftm $ putSt @action sing (ActionVal $ Left input)
  actionHandler'

mainHandler :: Op' Exit Main
mainHandler = I.do
  msg <- getInput
  case msg of
    EnterAdd _ -> I.do
      At et <- actionHandler ()
      liftm $ do
        allState . currIndex %= (+ 1)
        idx <- use (allState . currIndex)
        allState . entityList . at idx .= et
      mainHandler
    DeleteOne idx -> I.do
      getInput I.>>= \case
        Yes -> I.do
          liftm (allState . entityList . at idx .= Nothing)
          mainHandler
        No -> mainHandler
    EnterModify input -> I.do
      At output <- actionHandler input
      liftm $ case output of
        Nothing -> pure ()
        Just (i', et) -> allState . entityList . at i' .= Just et
      mainHandler
    EnterDeleteList input -> I.do
      At output <- actionHandler input
      liftm $ case output of
        Nothing -> pure ()
        Just (_, ls) ->
          for_ ls $ \i' ->
            allState . entityList . at i' .= Nothing
      mainHandler
    IsExitTodo ->
      getInput I.>>= \case
        Yes -> returnAt ()
        No -> mainHandler
    ExitTodo -> returnAt ()
