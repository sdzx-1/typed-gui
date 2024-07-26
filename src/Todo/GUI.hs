{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Todo.GUI where

import Control.Concurrent.STM
import Control.Monad
import Data.Bool.Singletons (SBool (..))
import Data.Dependent.Map qualified as D
import Data.Eq.Singletons (SEq (..))
import Data.IntMap qualified as IntMap
import Data.Singletons
import GHC.Conc
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Todo.Handler
import Todo.Type
import TypedFsm

sendSomeMsg :: TChan (AnyMsg ps) -> Sing t -> SomeMsg ps t -> UI ps t ()
sendSomeMsg tchan sfrom (SomeMsg sto msg) =
  liftIO $ atomically $ writeTChan tchan (AnyMsg sfrom sto msg)

testEnts :: [Entity]
testEnts =
  [ Entity Work Pending "hello1"
  , Entity Sports Done "hello2"
  , Entity Home Pending "hello3"
  ]

class RenderSt ps state where
  renderSt
    :: forall (t :: ps)
     . Sing t
    -> (TChan (AnyMsg ps), InternalStMap ps, state, Window)
    -> UI ps t (Maybe (Element, IO (ActionOutput t)))

instance MonadFail (UI ps t) where
  fail s = error s

mkEntity :: TChan (AnyMsg Todo) -> (Int, Entity) -> UI Todo Main Element
mkEntity tchan (i, entity@(Entity ttype status context')) = do
  delButton <- UI.button # set UI.text "Delete"
  on UI.click delButton $ \_ -> sendSomeMsg tchan SMain $ SomeMsg sing (DeleteOne i)
  modButton <- UI.button # set UI.text "Modify"
  on UI.click modButton $ \_ -> sendSomeMsg tchan SMain $ SomeMsg sing (EnterModify (i, entity))
  row
    [ UI.li # set UI.text (show ttype)
    , UI.li # set UI.text (show status)
    , UI.li # set UI.text context'
    , element delButton
    , element modButton
    ]

instance RenderSt Todo TodoList where
  renderSt sst resource@(tchan, intStMap, (TodoList _ entMap), w) = case sst of
    SMain -> do
      pure w # set title "Main"
      addButton <- UI.button # set UI.text "Add"
      on UI.click addButton $ \_ -> sendSomeMsg tchan SMain $ SomeMsg sing (EnterAdd ())
      cc <- column $ map (mkEntity tchan) (IntMap.toList entMap) ++ [element addButton]
      getBody w # set children [cc]
      pure Nothing
    SAreYouSure va vb -> do
      pure w # set title "Are You Sure?"
      yesButton <- UI.button # set UI.text "Yes"
      on UI.click yesButton $ (\_ -> sendSomeMsg tchan sst $ SomeMsg vb Yes)
      noButton <- UI.button # set UI.text "No"
      on UI.click noButton $ (\_ -> sendSomeMsg tchan sst $ SomeMsg va No)
      getBody w # set children [yesButton, noButton]
      pure Nothing
    SAction saction sfrom -> do
      pure w # set title ("Action " ++ show saction)
      Just (actPage, uiVal) <- liftIO $ runUI w $ renderSt saction resource
      subButton <- UI.button # set UI.text "submit!"
      on UI.click subButton $
        ( \_ -> do
            val <- liftIO uiVal
            sendSomeMsg tchan sst $
              SomeMsg
                (SAreYouSure sst sfrom)
                (SureAction val)
        )
      backButton <- UI.button # set UI.text "back"
      on UI.click backButton $
        (\_ -> sendSomeMsg tchan sst $ SomeMsg sfrom (ExitAction))
      getBody w # set children [actPage, subButton, backButton]
      pure Nothing
    SAdd -> do
      case D.lookup SAdd intStMap of
        Nothing -> error "np"
        Just (InternalSt instval) -> do
          typeSelect <- UI.select #+ [UI.option # set UI.value (show t) # set UI.text (show t) | t <- [Home .. Sports]]
          statusSelect <- UI.select #+ [UI.option # set UI.value (show t) # set UI.text (show t) | t <- [Pending, Done]]
          txtCon <- UI.input
          tdiv <- UI.div # set children [typeSelect, statusSelect, txtCon]
          case instval of
            Left _ -> pure ()
            Right (Entity typeV statusV st) -> void $ do
              element typeSelect # set UI.value (show typeV)
              element statusSelect # set UI.value (show statusV)
              element txtCon # set UI.value st
          let getVal = do
                runUI w $ do
                  typeV <- read <$> get UI.value typeSelect
                  statusV <- read <$> get UI.value statusSelect
                  descV <- get UI.value txtCon
                  pure (Entity typeV statusV descV)
          pure $ Just $ (tdiv, getVal)
    SModify -> do
      case D.lookup SModify intStMap of
        Nothing -> error "np"
        Just (InternalSt instval) -> do
          let (i', Entity typeV statusV descV) = fromEitherBoth instval
          typeSelect <-
            UI.select
              #+ [UI.option # set UI.value (show t) # set UI.text (show t) | t <- [Home .. Sports]]
              # set value (show typeV)
          statusSelect <-
            UI.select
              #+ [UI.option # set UI.value (show t) # set UI.text (show t) | t <- [Pending, Done]]
              # set value (show statusV)
          txtCon <- UI.input # set UI.value descV
          tdiv <- UI.div # set children [typeSelect, statusSelect, txtCon]
          let getVal = do
                runUI w $ do
                  typeV' <- read <$> get UI.value typeSelect
                  statusV' <- read <$> get UI.value statusSelect
                  descV' <- get UI.value txtCon
                  pure (i', (Entity typeV' statusV' descV'))
          pure $ Just $ (tdiv, getVal)
    v -> error (show v)

fromEitherBoth :: Either a a -> a
fromEitherBoth (Left a) = a
fromEitherBoth (Right a) = a

renderLoop
  :: forall ps state t
   . (RenderSt ps state, SEq ps)
  => StateRef ps state
  -> Window
  -> Sing (t :: ps)
  -> IO ()
renderLoop
  ref@StateRef{fsmStRef, internalStMapRef, stateRef, anyMsgTChan}
  window
  sst = do
    (SomeSing st) :: SomeSing ps <- atomically $ do
      SomeSing rsst <- readTVar fsmStRef
      case rsst %== sst of
        STrue -> retry
        SFalse -> pure (SomeSing rsst)
    internalSt <- readTVarIO internalStMapRef
    state <- readTVarIO stateRef
    runUI window $ renderSt st (anyMsgTChan, internalSt, state, window)
    renderLoop ref window st

setup :: TodoList -> Op' Exit Main -> Window -> UI Todo Main ()
setup state op' w = do
  nsRef@StateRef{anyMsgTChan} <-
    liftIO $ newStateRef SMain state
  liftIO $ forkIO $ void $ runHandler nsRef (Cont $ SomeOperate SMain op')
  liftIO $ forkIO $ void $ renderLoop nsRef w SMain
  renderSt SMain (anyMsgTChan, D.empty, state, w)
  pure ()

main :: IO ()
main = do
  startGUI
    defaultConfig
    ( setup
        ( TodoList 20 $
            IntMap.fromList $
              zip [0 ..] testEnts
        )
        mainHandler
    )