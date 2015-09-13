{-# LANGUAGE DeriveGeneric #-}

module BBrotSelection where

import Data.Aeson
import GHC.Generics

data BBPoint = BBPoint { pointX :: Double
                       , pointY :: Double
                       , orbitLength :: Int
                       }
               deriving (Eq, Show, Generic)

instance FromJSON BBPoint
instance ToJSON BBPoint

data PointSelection = PointSelection { pointList   :: [BBPoint]
                                     , commandLine :: Maybe [String]
                                     , randGen     :: Maybe String
                                     , timeStamp   :: Maybe String
                                     }
               deriving (Eq, Show, Generic)

instance FromJSON PointSelection
instance ToJSON PointSelection
