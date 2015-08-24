{-# LANGUAGE BangPatterns #-}

module BBrotRender(render) where

import Data.Complex
import Data.Array.IO
import Data.Word(Word32, Word8)
import Codec.Picture
import System.Console.CmdArgs

import BBrotConf

xmin = realPart loCorner
xmax = realPart hiCorner
ymin = imagPart loCorner
ymax = imagPart hiCorner

xrange = xmax - xmin
yrange = ymax - ymin

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


inWindow :: Complex Double -> Bool
inWindow z = x >= xmin && x < xmax && y >= ymin && y < ymax
  where x = realPart z
        y = imagPart z


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

plotPix :: IOUArray Int Word32 -> Int -> (Int, Int) -> IO ()
plotPix !img !xres (!x, !y) = do
  let !offset = y * xres + x
  v <- readArray img offset
  writeArray img offset (v + 1)

rel :: Double -> Double -> Double -> Double
rel !lo !range !v = (v - lo) / range

toPix :: Int -> Double -> Double -> Double -> Int
toPix !pixrange !realmin !realrange !a =
  floor $ (fromIntegral pixrange) * (rel realmin realrange a)

toImgCoords :: Int -> Int -> Complex Double -> (Int, Int)
toImgCoords !xres !yres !z =
    (toPix xres xmin xrange $ realPart z,
     toPix yres ymin yrange $ imagPart z)


render conf = do
  -- Load points of interest, render their orbits into a PNG image.
  whenNormal $ putStrLn $ "Loading cache " ++ icachepath conf ++ " ..."
  contents <- readFile $ icachepath conf
  let selected = map (read :: String -> Complex Double) $ lines contents
      orbits = concatMap orbs selected
      result = filter inWindow orbits
  whenNormal $ putStrLn $ "selected points: " ++ show (length selected)

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
