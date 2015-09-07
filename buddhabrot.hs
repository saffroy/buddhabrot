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

import BBrotConf
import BBrotCompute
import BBrotRender

-- Note: CmdArgs annotations are impure, they can be used only once
getConf = cmdArgs $ modes [
           Compute { seed       = Nothing           &= name "s"
                   , samples    = 1000 * 1000 * 500 &= name "n"
                   , minK       = 1000 * 1          &= name "k"
                   , maxK       = 1000 * 20         &= name "K"
                   , ocachepath = Nothing           &= name "c" &= typFile
                   , gridStep   = 0.05              &= name "g"
                   },
           Render { xpixels     = 1000              &= name "x"
                  , ypixels     = 1000              &= name "y"
                  , icachepath  = def               &= name "c" &= typFile
                  , imagepath   = Nothing           &= name "o" &= typFile
                  , palette     = Flames            &= name "p"
                  , curve       = Line              &= name "C"
                  },
           ShowCells { gridStep = 0.05              &= name "g"
                     , maxK     = 1000              &= name "K"
                     , animpath = "/tmp/cells.gif"  &= name "o" &= typFile
                  }
          ] &= program "buddhabrot" &= verbosity

main = do
  -- Note: CmdArgs annotations are impure, they can be used only once
  conf <- getConf

  case conf of
    conf@Compute{} -> compute conf
    conf@Render{} -> render conf
    conf@ShowCells{} -> showCells conf

  whenNormal $ putStrLn "Done!"
