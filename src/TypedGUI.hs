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

module TypedGUI where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (
  Chan,
  newChan,
  readChan,
  writeChan,
 )
import Control.Monad (void)
import Control.Monad.State (MonadIO (liftIO), StateT (runStateT))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Singletons.Base.TH (SEq, Sing, SomeSing (..))
import Graphics.UI.Threepenny (Element, UI, Window, runUI)
import TypedFsm (
  AnyMsg (..),
  Result (..),
  SomeMsg (..),
  SomeOperate (SomeOperate),
  UnexpectMsg (..),
  UnexpectMsgHandler (IgnoreAndTrace),
  getSomeOperateSing,
  runOperate,
 )

{-

Control status : cs
Data status    : ds

-}

type family RecRenderOutVal (t :: ps)

type RenderSt cs ds =
  forall (t :: cs)
   . Sing t
  -> ds
  -> Chan (AnyMsg cs)
  -> Window
  -> UI cs t (Maybe (Element, IO (RecRenderOutVal t)))

data InternalStRef cs ds = InternalStRef
  { dsRef :: IORef ds
  , csStRef :: IORef (SomeSing cs)
  , anyMsgTChan :: Chan (AnyMsg cs)
  }

newInternalStRef
  :: Sing (t :: cs)
  -> ds
  -> IO (InternalStRef cs ds)
newInternalStRef sst ds = do
  a <- newIORef ds
  b <- newIORef (SomeSing sst)
  c <- newChan
  pure (InternalStRef a b c)

runHandler
  :: (SEq cs)
  => InternalStRef cs ds
  -> Result cs (UnexpectMsg cs) (StateT ds IO) a
  -> IO (Result cs (UnexpectMsg cs) (StateT ds IO) a)
runHandler
  InternalStRef
    { dsRef
    , csStRef
    , anyMsgTChan
    }
  result = case result of
    Finish a -> pure (Finish a)
    e@(ErrorInfo (UnexpectMsg _)) -> pure e
    Cont (SomeOperate cssing op) -> do
      anyMsg <- readChan anyMsgTChan
      ds <- readIORef dsRef
      (newResult, ds') <-
        runStateT
          ( runOperate
              ( IgnoreAndTrace
                  (\_ -> liftIO $ putStrLn "recive unexpect msg!!")
              )
              [anyMsg]
              cssing
              op
          )
          ds
      writeIORef dsRef ds'
      case newResult of
        Cont sop -> do
          let st = getSomeOperateSing sop
          writeIORef csStRef $ SomeSing st
        _ -> pure ()
      pure newResult

sendSomeMsg
  :: Chan (AnyMsg cs)
  -> Sing (t :: cs)
  -> SomeMsg cs t
  -> UI cs t ()
sendSomeMsg tchan sfrom (SomeMsg sto msg) =
  liftIO $ writeChan tchan (AnyMsg sfrom sto msg)

renderUI
  :: forall cs ds
   . InternalStRef cs ds
  -> RenderSt cs ds
  -> Window
  -> IO ()
renderUI
  InternalStRef{dsRef, csStRef, anyMsgTChan}
  renderStFun
  window =
    do
      (SomeSing sst, ds) <- do
        somesst <- readIORef csStRef
        ds <- readIORef dsRef
        pure (somesst, ds)
      void $ runUI window $ renderStFun sst ds anyMsgTChan window

uiSetup
  :: (SEq cs)
  => InternalStRef cs ds
  -> RenderSt cs ds
  -> Result cs (UnexpectMsg cs) (StateT ds IO) ()
  -> Window
  -> UI ps (t :: ps) ()
uiSetup interStRef renderSt sthandler window = do
  let loop result = do
        liftIO $ renderUI interStRef renderSt window
        newResult <- liftIO $ runHandler interStRef result
        loop newResult
  Control.Monad.void $ liftIO $ forkIO $ Control.Monad.void $ loop sthandler