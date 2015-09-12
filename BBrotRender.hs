{-# LANGUAGE BangPatterns #-}

module BBrotRender(showCells, render) where

import Codec.Picture
import Control.Monad
import Data.Aeson
import Data.Array.IO
import qualified Data.ByteString.Lazy as BS
import Data.Complex
import Data.Maybe
import Data.Word(Word32, Word8)
import System.Console.CmdArgs
import System.Exit
import System.IO
import Text.Read

import BBrotCompute
import BBrotConf
import BBrotSelection

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

getPix :: IOUArray (Int, Int) Word32 -> (Word32 -> PixelRGB8) -> Int -> Int -> IO PixelRGB8
getPix img toRGB x y = do
  v <- readArray img (x, y)
  return $ toRGB v

plotPix :: IOUArray (Int, Int) Word32 -> (Int, Int) -> IO ()
plotPix !img (!x, !y) = do
  v <- readArray img (x, y)
  writeArray img (x, y) (v + 1)

rel :: Double -> Double -> Double -> Double
rel !lo !range !v = (v - lo) / range

toPix :: Int -> Double -> Double -> Double -> Int
toPix !pixrange !realmin !realrange !a =
  floor $ (fromIntegral pixrange) * (rel realmin realrange a)

toImgCoords :: Int -> Int -> Complex Double -> (Int, Int)
toImgCoords !xres !yres !z =
    (toPix xres xmin xrange $ realPart z,
     toPix yres ymin yrange $ imagPart z)

toPlaneCoords :: Int -> Int -> Int -> Int -> Complex Double
toPlaneCoords !xres !yres !i !j = x :+ y
    where x = xmin + xrange * fromIntegral i / fromIntegral xres
          y = ymin + yrange * fromIntegral j / fromIntegral yres

loadPointsJson :: String -> IO (Maybe PointSelection)
loadPointsJson filepath = do
  contents <- BS.readFile filepath
  return $ (decode contents :: Maybe PointSelection)

loadPointsComplex :: String -> IO (Maybe PointSelection)
loadPointsComplex filepath = do
  contents <- readFile filepath
  let zs = concatMap g $ lines contents
        where g = maybeToList . readMaybe :: String -> [Complex Double]
      ps = map f zs
        where f (x :+ y) = BBPoint x y 1
  return $ if null ps
           then Nothing
           else Just (PointSelection ps)

render conf = do
  -- Load points of interest, render their orbits into a PNG image.
  whenNormal $ putStrLn $ "Loading cache " ++ icachepath conf ++ " ..."
  maybePS <- if isComplex conf
             then loadPointsComplex $ icachepath conf
             else loadPointsJson $ icachepath conf
  when (maybePS == Nothing) $ do
    hPutStrLn stderr $ "failed to parse " ++ icachepath conf
    when (not $ isComplex conf) $ hPutStrLn stderr "try with flag -z"
    exitFailure

  let psel = pointList $ fromMaybe (PointSelection []) maybePS
      selected = map (\(BBPoint x y _) -> x :+ y) psel
      orbits = concatMap orbs selected
      result = filter inWindow $ if dontRender conf
                                 then selected
                                 else orbits
  whenNormal $ putStrLn $ "selected points: " ++ show (length selected)

  let xres = xpixels conf
      yres = ypixels conf
      coords = map (toImgCoords xres yres) result
  img <- newArray ((0, 0), (xres - 1, yres - 1)) 0 :: IO (IOUArray (Int, Int) Word32)
  mapM_ (plotPix img) coords
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
  ima <- withImage xres yres (getPix img pixFunc)
  writePng outfile ima


showCells conf = do
  -- Compute selection of cells (squares in the complex plane) that
  -- are close to the edge of the Mandelbrot set, and render them as
  -- animated GIF.
  let step = gridStep conf
      bailout = maxK conf
      xres = 1000
      yres = 1000
      !cells = selectCells step bailout
  whenNormal $ putStrLn $ "cell count: " ++ show (length cells)

  let red   = PixelRGB8 255 0 0
      black = PixelRGB8 0 0 0
      white = PixelRGB8 255 255 255

  whenNormal $ putStrLn "rendering cells"
  cellMap <- newArray ((0, 0), (xres - 1, yres - 1)) False :: IO (IOUArray (Int, Int) Bool)
  forM_ cells $ \(x, y) -> do
    let cornerA = (x - step / 2) :+ (y - step / 2)
        cornerB = (x + step / 2) :+ (y + step / 2)
        (imin, jmin) = toImgCoords xres yres cornerA
        (imax, jmax) = toImgCoords xres yres cornerB
        coords = [ (i, j) | i <- [imin..imax], j <- [jmin..jmax],
                                 i >= 0 && i < xres,
                                 j >= 0 && j < yres ]
    forM_ coords $ \(i, j) -> do
      writeArray cellMap (i, j) True

  let getCellPix :: Int -> Int -> IO PixelRGB8
      getCellPix i j = do
                   v <- readArray cellMap (i, j)
                   return $ if v then red else black
  imgCells <- withImage xres yres getCellPix

  whenNormal $ putStrLn "rendering mandel"
  let imgMandel = generateImage mandelRenderer xres yres
      inMandelbrotSet z = not $ inSet 0 bailout z
      mandelRenderer i j = if inMandelbrotSet $ toPlaneCoords xres yres i j
                          then white else black

  whenNormal $ putStrLn $ "writing " ++ animpath conf ++ " ..."
  case writeGifAnimation (animpath conf) 100 LoopingForever [imgMandel, imgCells] of
    Left err -> putStrLn $ "error generating gif animation: " ++ err
    Right action -> action
