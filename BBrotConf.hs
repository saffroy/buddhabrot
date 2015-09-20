{-# LANGUAGE DeriveDataTypeable #-}

module BBrotConf where

import Data.Complex
import System.Console.CmdArgs

loCorner, hiCorner :: Complex Double
loCorner = (-2.0) :+ (-1.5)
hiCorner =   1.0  :+   1.5

data ColorPalette = Gray | Reddish | Flames
                    deriving (Show, Data, Typeable)

data Curve = Line | Square | Root
             deriving (Show, Data, Typeable)

data BBrotConf = Compute { seed       :: Maybe Int
                         , samples    :: Int
                         , minIters   :: Int
                         , maxIters   :: Int
                         , ocachepath :: Maybe String
                         , gridStep   :: Double
                         }
               | Render { xpixels     :: Int
                        , ypixels     :: Int
                        , icachepath  :: String
                        , isComplex   :: Bool
                        , dontRender  :: Bool
                        , imagepath   :: Maybe String
                        , palette     :: ColorPalette
                        , curve       :: Curve
                        }
               | ShowCells { gridStep :: Double
                           , maxIters :: Int
                           , animpath :: String
                           }
                 deriving (Show, Data, Typeable)

