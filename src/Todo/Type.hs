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

import Control.Lens (makeLenses)
import Data.Dependent.Map (DMap)
import Data.GADT.Compare (GCompare (..), GEq (..))
import Data.IntMap (IntMap)
import Data.Singletons.Base.TH
import Data.Type.Equality (TestEquality (..))
import Todo.Common
import TypedFsm

$( singletons
    [d|
      data Todo
        = Main
        | Add
        | Delete
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
    DeleteOne
      :: Int
      -> Msg Todo Main (AreYouSure Main Main)
    -----------------
    IsExitTodo :: Msg Todo Main (AreYouSure Main Exit)
    ExitTodo :: Msg Todo Main Exit

type instance ActionInput Modify = (Int, Entity)
type instance ActionOutput Modify = (Int, Entity)
type instance ActionInput Add = ()
type instance ActionOutput Add = Entity

newtype ActionVal ps v = ActionVal (Either (ActionInput v) (ActionOutput v))

type ActionStMap ps = DMap (Sing @ps) (ActionVal ps)

instance GEq STodo where
  geq = testEquality

instance GCompare STodo where
  gcompare = sOrdToGCompare

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

makeLenses ''Entity
makeLenses ''TodoList
makeLenses ''AllState