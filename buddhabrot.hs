{-# LANGUAGE FlexibleInstances, BangPatterns #-}

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


samples = 1000 * 1000 * 500

minK =  1 * 1000
maxK = 20 * 1000
radius = 4

xpixels = 1000 :: Int
ypixels = 1000 :: Int
pixels = xpixels * ypixels

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

imgpath =
  printf "/tmp/buddhabrot-%s-%s_%s_id.png"
    (toUnit samples) (toUnit minK) (toUnit maxK)

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

toWindow :: Complex Double -> (Int, Int)
toWindow z = (toPix xpixels xrange $ realPart z, toPix ypixels yrange $ imagPart z)

inWindow :: Complex Double -> Bool
inWindow z = x >= xmin && x < xmax && y >= ymin && y < ymax
  where x = realPart z
        y = imagPart z

iterations :: Double -> Double -> Double -> Double -> Int -> Int
iterations !x !y !x0 !y0 !k =
  let x2 = x * x
      y2 = y * y
  in
   if k == maxK || x2 + y2 > radius
   then k
   else iterations (x2 - y2 + x0) (2 * x * y + y0) x0 y0 (k + 1)

inCardioBulb :: Double -> Double -> Bool
inCardioBulb !x !y = inCardio || inBulb
  where sqr a = a * a
        q = sqr (x - 1/4) + sqr y
        inCardio = q * (q + (x - 1/4)) < sqr y / 4
        inBulb = sqr (x + 1) + sqr y < 1 / 16

inSet :: Complex Double -> Bool
inSet !z = not (inCardioBulb x y) && k >= minK && k < maxK
  where x = realPart z
        y = imagPart z
        k = iterations x y x y 0

orbit :: Double -> Double -> Double -> Double -> [Complex Double] -> [Complex Double]
orbit x y x0 y0 l =
  let x2 = x * x
      y2 = y * y
      newx = (x2 - y2 + x0)
      newy = (2 * x * y + y0)
  in
   if x2 + y2 > radius
   then l
   else orbit newx newy x0 y0 (newx :+ newy : l)

orbs :: Complex Double -> [Complex Double]
orbs z = orbit x y x y []
  where x = realPart z
        y = imagPart z

plotPix :: IOUArray Int Word32 -> (Int, Int) -> IO ()
plotPix img (x, y) = do
  let offset = y * xpixels + x
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

getPix :: IOUArray Int Word32 -> Word32 -> Word32 -> Int -> Int -> IO PixelRGB8
getPix img smallest biggest x y = do
  v <- readArray img (x + xpixels * y)
  return $ toPixel smallest biggest v


main = do
  putStrLn $ printf "Sampling %d millions points..." $ samples `div` 1000000
  let seed = mkStdGen 0
  -- seed <- getStdGen
  
  let points = take samples $ randomRs (loCorner, hiCorner) seed
      selected = filter inSet points
      res = filter inWindow $ concat $ map orbs selected
  let coords = map toWindow res
  
  img <- newArray (0, pixels - 1) (0 :: Word32) :: IO (IOUArray Int Word32)
  sequence_ $ map (plotPix img) coords
  values <- getElems img
  
  putStrLn $ "Writing " ++ imgpath ++ " ..."
  let smallest = minimum values
      biggest  = maximum values
  ima <- withImage xpixels ypixels (getPix img smallest biggest)
  writePng imgpath ima
  putStrLn "Done!"
