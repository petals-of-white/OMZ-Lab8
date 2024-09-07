module Types where

data SimpleImage p = SimpleImage {imgPixels :: [p], imgRows :: Int, imgColumns :: Int}
    deriving (Eq, Show)

data DisplayMode = 
    DisplayFirst | DisplaySecond | DisplayFusionFirstSecond | DisplayFusionSecondFirst
    deriving (Eq, Show)

data AppModel p = AppModel { appImg1 :: SimpleImage p, appImg2 :: SimpleImage p,appDisplayMode :: DisplayMode}
  deriving (Eq, Show)

data AppEvent = AppInit | ApplyFirst | ApplySecond
  deriving (Eq, Show)
