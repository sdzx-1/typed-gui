{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Todo.Common where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State qualified as S
import Data.IORef
import Data.Singletons.Base.TH
import Graphics.UI.Threepenny (Element, UI, Window, runUI)
import TypedFsm

type R ps state otherState a =
  Result ps (UnexpectMsg ps) (S.StateT (AllState ps state otherState) IO) a

type family RenderOutVal (t :: ps)

type RenderSt ps state otherState =
  forall (t :: ps)
   . Sing t
  -> (Chan (AnyMsg ps), otherState, state, Window)
  -> UI ps t (Maybe (Element, IO (RenderOutVal t)))

data AllState ps state otherState = AllState
  { _allState :: state
  , _allOtherState :: otherState
  }

data StateRef ps state otherState = StateRef
  { stateRef :: IORef state
  , otherStateRef :: IORef otherState
  , fsmStRef :: IORef (SomeSing ps)
  , anyMsgTChan :: Chan (AnyMsg ps)
  }

newStateRef
  :: Sing (t :: ps)
  -> state
  -> otherState
  -> IO (StateRef ps state otherState)
newStateRef sst state otherState = do
  a <- newIORef state
  b <- newIORef otherState
  c <- newIORef (SomeSing sst)
  d <- newChan
  pure (StateRef a b c d)

runHandler
  :: (SEq ps, SingKind ps)
  => StateRef ps state otherState
  -> R ps state otherState a
  -> IO (R ps state otherState a)
runHandler
  StateRef
    { stateRef
    , otherStateRef
    , fsmStRef
    , anyMsgTChan
    }
  result = case result of
    Finish a -> pure (Finish a)
    e@(ErrorInfo (UnexpectMsg _)) -> pure e
    Cont (SomeOperate stsing op) -> do
      anyMsg <- readChan anyMsgTChan
      (state', internalStMap) <- do
        st' <- readIORef stateRef
        ist <- readIORef otherStateRef
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
      writeIORef stateRef a
      writeIORef otherStateRef b
      case res of
        Cont sop -> do
          let st = getSomeOperateSing sop
          writeIORef fsmStRef $ SomeSing st
        _ -> pure ()
      pure res

sendSomeMsg :: Chan (AnyMsg ps) -> Sing t -> SomeMsg ps t -> UI ps t ()
sendSomeMsg tchan sfrom (SomeMsg sto msg) =
  liftIO $ writeChan tchan (AnyMsg sfrom sto msg)

renderAll
  :: forall ps state otherState
   . StateRef ps state otherState
  -> RenderSt ps state otherState
  -> Window
  -> IO ()
renderAll
  StateRef{fsmStRef, otherStateRef, stateRef, anyMsgTChan}
  renderStFun
  window =
    do
      (SomeSing sst, state, otherState) <- do
        srsst <- readIORef fsmStRef
        state' <- readIORef stateRef
        otherState' <- readIORef otherStateRef
        pure (srsst, state', otherState')
      void $ runUI window $ renderStFun sst (anyMsgTChan, otherState, state, window)

uiSetup
  :: (SEq ps, SingKind ps)
  => StateRef ps state otherState
  -> RenderSt ps state otherState
  -> R ps state otherState ()
  -> Window
  -> UI ps (t :: ps) ()
uiSetup nsRef renderSt sthandler w = do
  let loop r = do
        liftIO $ renderAll nsRef renderSt w
        res <- liftIO $ runHandler nsRef r
        loop res
  void $ liftIO $ forkIO $ void $ loop sthandler