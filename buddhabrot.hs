{-# LANGUAGE FlexibleInstances, BangPatterns, DeriveDataTypeable #-}

{- buddhabrot reimplementation
   based on C source at: http://paulbourke.net/fractals/buddhabrot/
   see also:
   http://www.superliminal.com/fractals/bbrot/bbrot.htm
   http://erleuchtet.org/2010/07/ridiculously-large-buddhabrot.html
   http://www.steckles.com/buddha/
   http://softologyblog.wordpress.com/2011/06/26/buddhabrot-fractals/
   http://kindofdoon.blogspot.fr/2012/09/the-colored-orbit-buddhabrot.html
-}

import Data.Complex
import System.Random
import Data.Array.IO
import Data.Word(Word32, Word8)
import Text.Printf
import Codec.Picture hiding (Palette)
import System.Console.CmdArgs

data Palette = Gray | Reddish | Flames
                 deriving (Show, Data, Typeable)
data Curve = Line | Square | Root
                 deriving (Show, Data, Typeable)
data BBrotConf = Compute { seed       :: Maybe Int
                         , samples    :: Int
                         , minK       :: Int
                         , maxK       :: Int
                         , ocachepath :: Maybe String
                         }
               | Render { xpixels     :: Int
                        , ypixels     :: Int
                        , icachepath  :: String
                        , imagepath   :: Maybe String
                        , palette     :: Palette
                        , curve       :: Curve
                        }
                 deriving (Show, Data, Typeable)


toUnit n | n < 10^3  = show n ++ ""
         | n < 10^6  = show (n `div` 10^3) ++ "K"
         | n < 10^9  = show (n `div` 10^6) ++ "M"
         | otherwise = show (n `div` 10^9) ++ "G"


instance Random (Complex Double) where
  randomR (!loPoint, !hiPoint) g = (r :+ i, g2)
    where (r, g1) = randomR (realPart loPoint, realPart hiPoint) g
          (i, g2) = randomR (imagPart loPoint, imagPart hiPoint) g1
  random = randomR (0.0 :+ 0.0, 1.0 :+ 1.0)

loCorner = (-2.0) :+ (-1.5)
hiCorner =   1.0  :+   1.5

xmin = realPart loCorner
xmax = realPart hiCorner
ymin = imagPart loCorner
ymax = imagPart hiCorner

xrange = xmax - xmin
yrange = ymax - ymin


rel :: Double -> Double -> Double -> Double
rel !lo !range !v = (v - lo) / range

toPix :: Int -> Double -> Double -> Double -> Int
toPix !pixrange !realmin !realrange !a = floor $ (fromIntegral pixrange) * (rel realmin realrange a)

toImgCoords :: Int -> Int -> Complex Double -> (Int, Int)
toImgCoords !xres !yres !z =
    (toPix xres xmin xrange $ realPart z,
     toPix yres ymin yrange $ imagPart z)

inWindow :: Complex Double -> Bool
inWindow z = x >= xmin && x < xmax && y >= ymin && y < ymax
  where x = realPart z
        y = imagPart z

iterations :: Int -> Double -> Double -> Double -> Double -> Int -> Int
iterations !maxK !x !y !x0 !y0 !k =
  let x2 = x * x
      y2 = y * y
  in
   if k == maxK || x2 + y2 > 4
   then k
   else iterations maxK (x2 - y2 + x0) (2 * x * y + y0) x0 y0 (k + 1)

inCardioBulb :: Double -> Double -> Bool
inCardioBulb !x !y = inCardio || inBulb
  where sqr a = a * a
        q = sqr (x - 1/4) + sqr y
        inCardio = q * (q + (x - 1/4)) < sqr y / 4
        inBulb = sqr (x + 1) + sqr y < 1 / 16

inSet :: Int -> Int -> Complex Double -> Bool
inSet !minK !maxK !z = not (inCardioBulb x y) && k >= minK && k < maxK
  where x = realPart z
        y = imagPart z
        k = iterations maxK x y x y 0

orbit :: Double -> Double -> Double -> Double -> [Complex Double] -> [Complex Double]
orbit !x !y !x0 !y0 l =
  let !x2 = x * x
      !y2 = y * y
      !newx = (x2 - y2 + x0)
      !newy = (2 * x * y + y0)
  in
   if x2 + y2 > 4
   then l
   else orbit newx newy x0 y0 (newx :+ newy : l)

orbs :: Complex Double -> [Complex Double]
orbs z = orbit x y x y []
  where x = realPart z
        y = imagPart z

plotPix :: IOUArray Int Word32 -> Int -> (Int, Int) -> IO ()
plotPix !img !xres (!x, !y) = do
  let !offset = y * xres + x
  v <- readArray img offset
  writeArray img offset (v + 1)

norm :: (Double -> Double) -> Word32 -> Word32 -> Word32 -> Word8
norm curveFunc min max cnt = fromIntegral v
  where s = 2.0 * fromIntegral (cnt - min) / fromIntegral (max - min)
        t = if s > 1.0 then 1.0 else s
        u = curveFunc t
        v = floor $ u * 255

gray :: Word8 -> PixelRGB8
gray x = PixelRGB8 x x x

reddish :: Word8 -> PixelRGB8
reddish x = PixelRGB8 r g 0
  where xr = min x 127
        xg = min (x - xr) 127
        r = 2 * xr
        g = 2 * xg

flames :: Word8 -> PixelRGB8
flames x = PixelRGB8 r g b
  where xr = min 120 x
        xg = min 120 (x - xr)
        xb = min 15 (x - xr - xg)
        r = xr * 2 + xr `div` 10
        g = xg * 2 + xg `div` 10
        b = xb * 17

toPixel :: (Double -> Double) -> Word32 -> Word32 -> (Word8 -> PixelRGB8) -> Word32 -> PixelRGB8
toPixel curveFunc smallest biggest scheme = scheme . norm curveFunc smallest biggest

getPix :: IOUArray Int Word32 -> Int -> (Word32 -> PixelRGB8) -> Int -> Int -> IO PixelRGB8
getPix img xres toRGB x y = do
  v <- readArray img (x + xres * y)
  return $ toRGB v

main = do
  -- Note: CmdArgs annotations are impure, they can be used only once
  conf <- cmdArgs $ modes [
           Compute { seed       = Nothing           &= name "s"
                   , samples    = 1000 * 1000 * 500 &= name "n"
                   , minK       = 1000 * 1          &= name "k"
                   , maxK       = 1000 * 20         &= name "K"
                   , ocachepath = Nothing           &= name "c" &= typFile
                   },
           Render { xpixels     = 1000              &= name "x"
                  , ypixels     = 1000              &= name "y"
                  , icachepath  = def               &= name "c" &= typFile
                  , imagepath   = Nothing           &= name "o" &= typFile
                  , palette     = Flames            &= name "p"
                  , curve       = Line              &= name "C"
                  }
          ] &= program "buddhabrot" &= verbosity

  case conf of
    conf@(Compute _ _ _ _ _) -> do
      -- Pick random points in the complex plane, test their orbits,
      -- save the interesting ones.

      whenNormal $ putStrLn $ printf "Sampling %s points..." $ toUnit $ samples conf
      randGen <- case seed conf of
        Just s -> return $ mkStdGen s
        Nothing -> getStdGen

      let points = take (samples conf) $ randomRs (loCorner, hiCorner) randGen
          selected = filter p points
            where p = inSet (minK conf) (maxK conf)
          cachefile = case ocachepath conf of
            Just s -> s
            Nothing -> printf "/tmp/buddhabrot-%s-%s_%s.bbc"
                       (toUnit $ samples conf)
                       (toUnit $ minK conf)
                       (toUnit $ maxK conf)

      whenNormal $ putStrLn $ "Writing cache " ++ cachefile ++ " ..."
      writeFile cachefile $ unlines . map show $ selected
      whenLoud $ putStrLn $ "selected points: " ++ show (length selected)

    conf@(Render _ _ _ _ _ _) -> do
      -- Load points of interest, render their orbits into a PNG image.
      whenNormal $ putStrLn $ "Loading cache " ++ icachepath conf ++ " ..."
      contents <- readFile $ icachepath conf
      let selected = map (read :: String -> Complex Double) $ lines contents
          orbits = concatMap orbs selected
          result = filter inWindow orbits
      whenLoud $ putStrLn $ "selected points: " ++ show (length selected)

      let xres = xpixels conf
          yres = ypixels conf
          nPixels = xres * yres
          coords = map (toImgCoords xres yres) result
      img <- newArray (0, nPixels - 1) (0 :: Word32) :: IO (IOUArray Int Word32)
      mapM_ (plotPix img xres) coords
      values <- getElems img
      whenLoud $ putStrLn $ "img points: " ++ show (sum values)

      let outfile = case imagepath conf of
            Just s -> s
            Nothing -> icachepath conf ++ ".png"
      whenNormal $ putStrLn $ "Writing " ++ outfile ++ " ..."
      let !smallest = minimum values
          !biggest  = maximum values
          colorScheme = case palette conf of
            Flames -> flames
            Gray -> gray
            Reddish -> reddish
          curveFunc = case curve conf of
            Line -> id
            Root -> sqrt
            Square -> (**2)
          pixFunc = toPixel curveFunc smallest biggest colorScheme
      ima <- withImage xres yres (getPix img xres pixFunc)
      writePng outfile ima

  whenNormal $ putStrLn "Done!"
