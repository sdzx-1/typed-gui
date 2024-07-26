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

import Control.Concurrent.STM
import Control.Exception
import Control.Lens (at, use)
import Control.Lens.Setter ((%=), (.=))
import Control.Monad.State
import qualified Control.Monad.State as S
import qualified Data.Dependent.Map as DMap
import Data.Eq.Singletons
import Data.IFunctor (At (..), returnAt)
import qualified Data.IFunctor as I
import Data.Singletons
import Todo.Type
import TypedFsm

type Op' to from = Op Todo (AllState Todo TodoList) IO () to from

getSt :: Sing (st :: Todo) -> StateT (AllState Todo TodoList) IO (Maybe (InternalSt Todo st))
getSt st = do
  dp <- use allInternalStMap
  case DMap.lookup st dp of
    Nothing -> pure Nothing
    Just v -> pure $ Just v

putSt :: Sing (st :: Todo) -> (InternalSt Todo st) -> StateT (AllState Todo TodoList) IO ()
putSt st sval =
  allInternalStMap %= DMap.alter (const (Just sval)) st

actionHandler'
  :: forall action to
   . (SingI to, SingI action)
  => Op Todo (AllState Todo TodoList) IO (Maybe (ActionOutput action)) to (Action action to)
actionHandler' =
  getInput I.>>= \case
    SureAction val ->
      getInput I.>>= \case
        Yes -> returnAt (Just val)
        No -> I.do
          liftm $ putSt @action (sing @action) (InternalSt $ Right val)
          actionHandler'
    ExitAction -> returnAt Nothing

actionHandler
  :: forall action to
   . (SingI to, SingI action)
  => ActionInput action
  -> Op Todo (AllState Todo TodoList) IO (Maybe (ActionOutput action)) to (Action action to)
actionHandler input = I.do
  liftm $ putSt @action (sing @action) (InternalSt $ Left input)
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
      case output of
        Nothing -> mainHandler
        Just (i', et) -> I.do
          liftm (allState . entityList . at i' .= Just et)
          mainHandler
    IsExitTodo ->
      getInput I.>>= \case
        Yes -> returnAt ()
        No -> mainHandler
    ExitTodo -> returnAt ()

runHandler
  :: (SingKind ps, SEq ps)
  => StateRef ps state
  -> Result ps (UnexpectMsg ps) (S.StateT (AllState ps state) IO) a
  -> IO a
runHandler
  stRef@StateRef
    { stateRef
    , internalStMapRef
    , fsmStRef
    , anyMsgTChan
    }
  result = case result of
    Finish a -> pure a
    ErrorInfo (UnexpectMsg _) -> throwIO UMsg
    Cont sop@(SomeOperate stsing op) -> do
      let st = getSomeOperateSing sop
      atomically $ writeTVar fsmStRef $ SomeSing st
      anyMsg <- atomically $ readTChan anyMsgTChan
      (state', internalStMap) <- atomically $ do
        st' <- readTVar stateRef
        ist <- readTVar internalStMapRef
        pure (st', ist)
      (res, AllState a b) <-
        S.runStateT
          ( runOperate
              (IgnoreAndTrace (\_ -> liftIO $ putStrLn "recive unexpect msg!!"))
              [anyMsg]
              stsing
              op
          )
          (AllState state' internalStMap)
      atomically $ do
        writeTVar stateRef a
        writeTVar internalStMapRef b
      runHandler stRef res