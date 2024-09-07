{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
module App where

import           Data.Word    (Word8)
import           Linear
import           Log          (cleanLog)
import           Monomer
import           OpenGLWidget
import           Types

handleEvent
  :: WidgetEnv (AppModel p) AppEvent
  -> WidgetNode (AppModel p) AppEvent
  -> AppModel p
  -> AppEvent
  -> [AppEventResponse (AppModel p) AppEvent]


handleEvent _wenv _node model evt = case evt of
  AppInit -> [Task (cleanLog >>= const (return ApplyFirst))]
  ApplyFirst ->
    case appDisplayMode model of
      DisplayFirst -> []
      DisplaySecond -> [Model model {appDisplayMode = DisplayFusionSecondFirst}]
      DisplayFusionFirstSecond -> [Model model {appDisplayMode=DisplayFirst}]
      DisplayFusionSecondFirst -> [Model model {appDisplayMode=DisplayFirst}]

  ApplySecond ->

    case appDisplayMode model of
      DisplayFirst -> [Model model {appDisplayMode = DisplayFusionFirstSecond}]
      DisplaySecond -> []
      DisplayFusionFirstSecond -> [Model model {appDisplayMode=DisplaySecond}]
      DisplayFusionSecondFirst -> [Model model {appDisplayMode=DisplaySecond}]

buildUI
  ::
  WidgetEnv (AppModel Word8)  AppEvent
  -> AppModel Word8
  -> WidgetNode (AppModel Word8) AppEvent

buildUI _wenv AppModel {appImg1=img1, appImg2=img2, appDisplayMode=displayMode} = widgetTree where
  widgetTree = keystroke [("1", ApplyFirst), ("2", ApplySecond)] (openGLWidget img1 img2 displayMode transMat)
  transMat = identity
