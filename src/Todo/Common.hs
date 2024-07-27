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

module Todo.Common where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State qualified as S
import Data.Data
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as D
import Data.Singletons.Base.TH
import TypedFsm

type family InterSt (v :: ps)

newtype InternalSt ps v = InternalSt (InterSt v)

type InternalStMap ps = DMap (Sing @ps) (InternalSt ps)

data AllState ps state = AllState
  { _allState :: state
  , _allInternalStMap :: (InternalStMap ps)
  }

data StateRef ps state = StateRef
  { stateRef :: TVar state
  , internalStMapRef :: TVar (InternalStMap ps)
  , fsmStRef :: TVar (SomeSing ps)
  , anyMsgTChan :: TChan (AnyMsg ps)
  }

newStateRef :: Sing (t :: ps) -> state -> IO (StateRef ps state)
newStateRef sst state = do
  a <- newTVarIO state
  b <- newTVarIO D.empty
  c <- newTVarIO (SomeSing sst)
  d <- newTChanIO
  pure (StateRef a b c d)

data UMsg = UMsg
  deriving (Typeable, Show, Exception)

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