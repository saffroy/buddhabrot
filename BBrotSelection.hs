{-# LANGUAGE DeriveGeneric #-}

module BBrotSelection where

import Control.Monad.Par
import Data.Aeson(FromJSON, ToJSON)
import GHC.Generics

data BBPoint = BBPoint { pointX :: Double
                       , pointY :: Double
                       , orbitLength :: Int
                       }
               deriving (Eq, Show, Generic)

instance FromJSON BBPoint
instance ToJSON BBPoint
instance NFData BBPoint


data PointSelection = PointSelection { pointList   :: [BBPoint]
                                     , commandLine :: Maybe [String]
                                     , randGen     :: Maybe String
                                     , timeStamp   :: Maybe String
                                     }
               deriving (Eq, Show, Generic)

instance FromJSON PointSelection
instance ToJSON PointSelection
