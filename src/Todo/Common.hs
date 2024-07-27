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
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Todo.Common where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State qualified as S
import Data.Data
import Data.Singletons.Base.TH
import Graphics.UI.Threepenny (Element, UI, Window, runUI)
import TypedFsm

data AllState ps state otherState = AllState
  { _allState :: state
  , _allOtherState :: otherState
  }

data StateRef ps state otherState = StateRef
  { stateRef :: TVar state
  , otherStateRef :: TVar otherState
  , fsmStRef :: TVar (SomeSing ps)
  , anyMsgTChan :: TChan (AnyMsg ps)
  }

newStateRef
  :: Sing (t :: ps)
  -> state
  -> otherState
  -> IO (StateRef ps state otherState)
newStateRef sst state otherState = do
  a <- newTVarIO state
  b <- newTVarIO otherState
  c <- newTVarIO (SomeSing sst)
  d <- newTChanIO
  pure (StateRef a b c d)

data UMsg = UMsg
  deriving (Typeable, Show, Exception)

runHandler
  :: (SingKind ps, SEq ps)
  => StateRef ps state otherState
  -> Result ps (UnexpectMsg ps) (S.StateT (AllState ps state otherState) IO) a
  -> IO a
runHandler
  stRef@StateRef
    { stateRef
    , otherStateRef
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
        ist <- readTVar otherStateRef
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
        writeTVar otherStateRef b
      runHandler stRef res

type family RenderOutVal (t :: ps)

type RenderSt ps state otherState =
  forall (t :: ps)
   . Sing t
  -> (TChan (AnyMsg ps), otherState, state, Window)
  -> UI ps t (Maybe (Element, IO (RenderOutVal t)))

sendSomeMsg :: TChan (AnyMsg ps) -> Sing t -> SomeMsg ps t -> UI ps t ()
sendSomeMsg tchan sfrom (SomeMsg sto msg) =
  liftIO $ atomically $ writeTChan tchan (AnyMsg sfrom sto msg)

type TestEqForState state = state -> state -> Bool
type TestEqForOtherState ps otherState = forall (t :: ps). Sing t -> otherState -> otherState -> Bool

renderLoop
  :: forall ps state otherState t
   . (SEq ps)
  => TestEqForState state
  -> TestEqForOtherState ps otherState
  -> RenderSt ps state otherState
  -> StateRef ps state otherState
  -> Window
  -> Sing (t :: ps)
  -> state
  -> otherState
  -> IO ()
renderLoop
  testEqForState
  testEqForOtherState
  renderStFun
  ref@StateRef{fsmStRef, otherStateRef, stateRef, anyMsgTChan}
  window
  sst
  state
  otherState = do
    runUI window $ renderStFun sst (anyMsgTChan, otherState, state, window)
    (SomeSing sst', state', otherState') <- atomically $ do
      srsst@(SomeSing sst') <- readTVar fsmStRef
      state' <- readTVar stateRef
      otherState' <- readTVar otherStateRef
      let res = (srsst, state', otherState')
      case sst' %== sst of
        SFalse -> pure res
        STrue ->
          if testEqForState state state'
            && testEqForOtherState sst otherState otherState'
            then retry
            else pure res
    renderLoop
      testEqForState
      testEqForOtherState
      renderStFun
      ref
      window
      sst'
      state'
      otherState'

renderLoopOnlySt
  :: forall ps state otherState t
   . (SEq ps)
  => RenderSt ps state otherState
  -> StateRef ps state otherState
  -> Window
  -> Sing (t :: ps)
  -> state
  -> otherState
  -> IO ()
renderLoopOnlySt = renderLoop (\_ _ -> True) (\_ _ _ -> True)