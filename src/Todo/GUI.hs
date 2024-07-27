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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Todo.GUI where

import Control.Concurrent.STM
import Control.Monad
import Data.Dependent.Map qualified as D
import Data.IntMap qualified as IntMap
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Singletons
import Data.Traversable (for)
import GHC.Conc
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Todo.Common
import Todo.Handler
import Todo.Type
import TypedFsm

testEnts :: [Entity]
testEnts =
  [ Entity Work Pending "hello1"
  , Entity Sports Done "hello2"
  , Entity Home Pending "hello3"
  ]

type instance RenderOutVal (t :: Todo) = ActionOutput t

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

renderSt :: RenderSt Todo TodoList (ActionStMap Todo)
renderSt sst resource@(tchan, otherSt, (TodoList _ entMap), w) = case sst of
  SMain -> do
    pure w # set title "Main"
    addButton <- UI.button # set UI.text "Add"
    on UI.click addButton $ \_ -> sendSomeMsg tchan SMain $ SomeMsg sing (EnterAdd ())
    delButton <- UI.button # set UI.text "DeleteSome"
    on UI.click delButton $ \_ ->
      sendSomeMsg tchan SMain $ SomeMsg sing (EnterDeleteList (IntMap.keys entMap))
    cc <-
      column $
        map (mkEntity tchan) (IntMap.toList entMap)
          ++ [ element delButton
             , element addButton
             ]
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
    subButton <- UI.button # set UI.text "submit! need confirmation"
    on UI.click subButton $
      ( \_ -> do
          val <- liftIO uiVal
          sendSomeMsg tchan sst $
            SomeMsg
              (SAreYouSure sst sfrom)
              (SureAction val)
      )

    subButton1 <- UI.button # set UI.text "submit! Don't need confirmation"
    on UI.click subButton1 $
      ( \_ -> do
          val <- liftIO uiVal
          sendSomeMsg tchan sst $
            SomeMsg
              sfrom
              (SubAction val)
      )
    backButton <- UI.button # set UI.text "back"
    on UI.click backButton $
      (\_ -> sendSomeMsg tchan sst $ SomeMsg sfrom (ExitAction))
    getBody w # set children [actPage, subButton, subButton1, backButton]
    pure Nothing
  SAdd -> do
    case D.lookup SAdd otherSt of
      Nothing -> error "np"
      Just (ActionVal instval) -> do
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
    case D.lookup SModify otherSt of
      Nothing -> error "np"
      Just (ActionVal instval) -> do
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
  SDelete -> do
    case D.lookup SDelete otherSt of
      Nothing -> error "np"
      Just (ActionVal ls') -> do
        let (ls, checkSet) = fromActOutDelete ls'
        checkBoxList <- do
          for ls $ \i -> do
            checkBox <-
              UI.input
                # set UI.type_ "checkbox"
                # set UI.value (show i)
                # set UI.checked (if i `Set.member` checkSet then True else False)
            lab <- UI.li # set text (show $ fromJust $ IntMap.lookup i entMap)
            pure (checkBox, row [element checkBox, element lab])
        let (refs, els) = unzip checkBoxList
        ee <- column els
        let getVal = do
              runUI w $
                concat
                  <$> for
                    refs
                    ( \ref -> do
                        v1 <- get UI.checked ref
                        v2 <- read @Int <$> get UI.value ref
                        if v1
                          then pure [v2]
                          else pure []
                    )
        pure (Just (ee, ((ls,) <$> getVal)))
  v -> error (show v)

fromEitherBoth :: Either a a -> a
fromEitherBoth (Left a) = a
fromEitherBoth (Right a) = a

fromActOutDelete :: Either [Int] ([Int], [Int]) -> ([Int], Set Int)
fromActOutDelete (Left a) = (a, Set.empty)
fromActOutDelete (Right (a, ls)) = (a, Set.fromList ls)

setup :: TodoList -> Op' Exit Main -> Window -> UI Todo Main ()
setup state op' w = do
  nsRef@StateRef{} <- liftIO $ newStateRef SMain state D.empty
  liftIO $ forkIO $ void $ runHandler nsRef (Cont $ SomeOperate SMain op')
  liftIO $ forkIO $ void $ renderLoopOnlySt renderSt nsRef w SMain state D.empty
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