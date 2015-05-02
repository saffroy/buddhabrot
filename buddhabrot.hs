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
import qualified  Data.ByteString.Lazy as L
import Text.Printf
import Codec.Picture
import System.Console.CmdArgs

data BBrotConf = BBrotConf {
      seed    :: Maybe Int
    , samples :: Int
    , minK    :: Int
    , maxK    :: Int
    , xpixels :: Int
    , ypixels :: Int
    , output  :: Maybe String
  } deriving (Show, Data, Typeable)

loCorner = (-2.0) :+ (-1.5)
hiCorner =   1.0  :+   1.5

colorScheme = flames
normFunc = id -- or sqrt or (**2)

--

instance Random (Complex Double) where
  randomR (!loPoint, !hiPoint) g = (r :+ i, g2)
    where (r, g1) = randomR (realPart loPoint, realPart hiPoint) g
          (i, g2) = randomR (imagPart loPoint, imagPart hiPoint) g1
  random = randomR (0.0 :+ 0.0, 1.0 :+ 1.0)

toUnit :: Int -> String
toUnit n | n < 10^3 = show n ++ ""
         | n < 10^6 = show (n `div` 10^3) ++ "K"
         | n < 10^9 = show (n `div` 10^6) ++ "M"
         | otherwise = show (n `div` 10^9) ++ "G"

imgpath conf =
    case output conf of
      Just path -> path
      Nothing -> printf "/tmp/buddhabrot-%s-%s_%s_id.png"
                     (toUnit $ samples conf) (toUnit $ minK conf) (toUnit $ maxK conf)

xmin = realPart loCorner
xmax = realPart hiCorner
ymin = imagPart loCorner
ymax = imagPart hiCorner

xrange = (xmin, xmax)
yrange = (ymin, ymax)


rel :: (Double, Double) -> Double -> Double
rel (lo, hi) v = (v - lo) / (hi - lo)

toPix :: Int -> (Double, Double) -> Double -> Int
toPix pixrange realrange a = floor $ (fromIntegral pixrange) * (rel realrange a)

toImgCoords :: Int -> Int -> Complex Double -> (Int, Int)
toImgCoords xres yres z =
    (toPix xres xrange $ realPart z,
     toPix yres yrange $ imagPart z)

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
orbit x y x0 y0 l =
  let x2 = x * x
      y2 = y * y
      newx = (x2 - y2 + x0)
      newy = (2 * x * y + y0)
  in
   if x2 + y2 > 4
   then l
   else orbit newx newy x0 y0 (newx :+ newy : l)

orbs :: Complex Double -> [Complex Double]
orbs z = orbit x y x y []
  where x = realPart z
        y = imagPart z

plotPix :: IOUArray Int Word32 -> Int -> (Int, Int) -> IO ()
plotPix img xres (x, y) = do
  let offset = y * xres + x
  v <- readArray img offset
  writeArray img offset (v + 1)

norm :: Word32 -> Word32 -> Word32 -> Word8
norm min max cnt = fromIntegral v
  where s = 2.0 * fromIntegral (cnt - min) / fromIntegral (max - min)
        t = if s > 1.0 then 1.0 else s
        u = normFunc t
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

toPixel :: Word32 -> Word32 -> Word32 -> PixelRGB8
toPixel smallest biggest = colorScheme . norm smallest biggest

getPix :: IOUArray Int Word32 -> Int -> Word32 -> Word32 -> Int -> Int -> IO PixelRGB8
getPix img xres smallest biggest x y = do
  v <- readArray img (x + xres * y)
  return $ toPixel smallest biggest v

main = do
  -- Note: CmdArgs annotations are impure, they can be used only once
  conf <- cmdArgs $ BBrotConf {
                     seed     = Nothing           &= name "s"
                   , samples  = 1000 * 1000 * 500 &= name "n"
                   , minK     = 1000 * 1          &= name "m"
                   , maxK     = 1000 * 20         &= name "M"
                   , xpixels  = 1000              &= name "x"
                   , ypixels  = 1000              &= name "y"
                   , output   = Nothing           &= name "o"
                   } &= program "buddhabrot"

  putStrLn $ printf "Sampling %s points..." $ toUnit $ samples conf
  randGen <- case seed conf of
               Nothing -> getStdGen
               Just s -> return $ mkStdGen s

  let points = take (samples conf) $ randomRs (loCorner, hiCorner) randGen
      selected = filter p points
          where p = inSet (minK conf) (maxK conf)
      result = filter inWindow $ concat $ map orbs selected

  let xres = xpixels conf
      yres = ypixels conf
      nPixels = xres * yres
      coords = map (toImgCoords xres yres) result
  img <- newArray (0, nPixels - 1) (0 :: Word32) :: IO (IOUArray Int Word32)
  sequence_ $ map (plotPix img xres) coords
  values <- getElems img

  let outfile = imgpath conf
  putStrLn $ "Writing " ++ outfile ++ " ..."
  let smallest = minimum values
      biggest  = maximum values
  ima <- withImage xres yres (getPix img xres smallest biggest)
  writePng outfile ima
  putStrLn "Done!"
