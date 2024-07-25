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

module Todo.Type where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens (makeLenses)
import Data.Data
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as D
import Data.GADT.Compare (GCompare (..), GEq (..))
import Data.IntMap (IntMap)
import Data.Singletons.Base.TH
import Data.Type.Equality (TestEquality (..))
import TypedFsm

$( singletons
    [d|
      data Todo
        = Main
        | Add
        | Delect
        | Modify
        | Exit
        | Action Todo Todo
        | AreYouSure Todo Todo
        deriving (Show, Eq, Ord)
      |]
 )

type family ActionInput (a :: ts)
type family ActionOutput (a :: ts)

instance StateTransMsg Todo where
  data Msg Todo form to where
    Yes :: Msg Todo (AreYouSure from to) to
    No :: Msg Todo (AreYouSure from to) from
    ------------
    ExitAction :: Msg Todo (Action action from) from
    SureAction
      :: ActionOutput action
      -> Msg
          Todo
          (Action action from)
          (AreYouSure (Action action from) from)
    --------------
    EnterAdd
      :: ActionInput Add
      -> Msg Todo Main (Action Add Main)
    EnterModify
      :: ActionInput Modify
      -> Msg Todo Main (Action Modify Main)
    DelectOne
      :: Int
      -> Msg Todo Main (AreYouSure Main Main)
    -----------------
    IsExitTodo :: Msg Todo Main (AreYouSure Main Exit)
    ExitTodo :: Msg Todo Main Exit

type instance ActionInput Modify = (Int, Entity)
type instance ActionOutput Modify = (Int, Entity)
type instance ActionInput Add = ()
type instance ActionOutput Add = Entity

instance GEq STodo where
  geq = testEquality

instance GCompare STodo where
  gcompare = sOrdToGCompare

type family InterSt (v :: ps)

type instance
  InterSt (s :: Todo) =
    Either (ActionInput s) (ActionOutput s)

newtype InternalSt ps v = InternalSt (InterSt v)

type InternalStMap ps = DMap (Sing @ps) (InternalSt ps)

data AllState ps state = AllState
  { _allState :: state
  , _allInternalStMap :: (InternalStMap ps)
  }

data Entity = Entity
  { _todoType :: TodoType
  , _todoStatus :: TodoStatus
  , _todoDescription :: String
  }
  deriving (Eq, Show)

data TodoStatus
  = Pending
  | Done
  deriving (Eq, Read, Show, Enum)

data TodoType
  = Home
  | Work
  | Sports
  deriving (Eq, Read, Show, Enum)

data TodoList = TodoList
  { _currIndex :: Int
  , _entityList :: IntMap Entity
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

makeLenses ''Entity
makeLenses ''TodoList
makeLenses ''AllState