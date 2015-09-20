{- buddhabrot reimplementation
   based on C source at: http://paulbourke.net/fractals/buddhabrot/
   see also:
   http://www.superliminal.com/fractals/bbrot/bbrot.htm
   http://erleuchtet.org/2010/07/ridiculously-large-buddhabrot.html
   http://www.steckles.com/buddha/
   http://softologyblog.wordpress.com/2011/06/26/buddhabrot-fractals/
   http://kindofdoon.blogspot.fr/2012/09/the-colored-orbit-buddhabrot.html
-}

{-# OPTIONS_GHC -fno-cse #-} -- required by cmdArgs :-(

module Main(main) where

import System.Console.CmdArgs

import BBrotCompute
import BBrotConf
import BBrotRender

-- Note: CmdArgs annotations are impure, they can be used only once
getConf :: IO BBrotConf
getConf = cmdArgs $ modes [
           Compute { seed       = Nothing           &= name "s"
                   , samples    = 1000 * 1000 * 500 &= name "n"
                   , minIters   = 1000 * 1          &= name "k"
                   , maxIters   = 1000 * 20         &= name "K"
                   , ocachepath = Nothing           &= name "c" &= typFile
                   , gridStep   = 0.01              &= name "g"
                   },
           Render { xpixels     = 1000              &= name "x"
                  , ypixels     = 1000              &= name "y"
                  , icachepath  = def               &= name "c" &= typFile
                  , isComplex   = False             &= name "z"
                  , dontRender  = False             &= name "r"
                  , imagepath   = Nothing           &= name "o" &= typFile
                  , palette     = Flames            &= name "p"
                  , curve       = Line              &= name "C"
                  },
           ShowCells { gridStep = 0.01              &= name "g"
                     , maxIters = 1000              &= name "K"
                     , animpath = "cells.gif"       &= name "o" &= typFile
                  }
          ] &= program "buddhabrot" &= verbosity

main :: IO ()
main = do
  -- Note: CmdArgs annotations are impure, they can be used only once
  conf <- getConf

  case conf of
    Compute{}   -> compute conf
    Render{}    -> render conf
    ShowCells{} -> showCells conf

  whenNormal $ putStrLn "Done!"
