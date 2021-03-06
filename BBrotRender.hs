{-# LANGUAGE BangPatterns #-}

module BBrotRender(showCells, render) where

import Codec.Picture
import Control.Monad
import Data.Aeson(decode)
import Data.Array.IO
import Data.Array.Unboxed
import qualified Data.ByteString.Lazy as BS
import Data.Complex
import Data.Maybe
import Data.List
import Data.Word(Word32, Word8)
import System.Console.CmdArgs(whenNormal)
import System.Exit
import System.IO
import Text.Read(readMaybe)

import BBrotCompute
import BBrotConf
import BBrotSelection

xmin, xmax, ymin, ymax :: Double
xmin = realPart loCorner
xmax = realPart hiCorner
ymin = imagPart loCorner
ymax = imagPart hiCorner

xrange, yrange :: Double
xrange = xmax - xmin
yrange = ymax - ymin

orbs :: Complex Double -> [Complex Double]
orbs !(x0 :+ y0) = unfoldr f (x0, y0)
  where f :: (Double, Double) -> Maybe (Complex Double, (Double, Double))
        f (!x, !y) =
          let !x2 = x * x
              !y2 = y * y
              !newx = (x2 - y2 + x0)
              !newy = (2 * x * y + y0)
          in
           if x2 + y2 > 4
           then Nothing
           else Just (newx :+ newy, (newx, newy))

inWindow :: Complex Double -> Bool
inWindow z = x >= xmin && x < xmax && y >= ymin && y < ymax
  where x = realPart z
        y = imagPart z


norm :: (Double -> Double) -> Word32 -> Word32 -> Word32 -> Word8
norm curveFunc minV maxV cnt = v
  where t = fromIntegral (cnt - minV) / fromIntegral (maxV - minV)
        u = curveFunc $ min 1.0 (2.0 * t)
        v = floor $ u * 255 :: Word8


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

plotPix :: IOUArray (Int, Int) Word32 -> (Int, Int) -> IO ()
plotPix !img (!x, !y) = do
  v <- readArray img (x, y)
  writeArray img (x, y) (v + 1)

rel :: Double -> Double -> Double -> Double
rel !lo !realrange !v = (v - lo) / realrange

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

emptyPS :: PointSelection
emptyPS = PointSelection { pointList   = []
                         , commandLine = Nothing
                         , randGen     = Nothing
                         , timeStamp   = Nothing
                         }

loadPointsJson :: String -> IO (Maybe PointSelection)
loadPointsJson filepath = do
  contents <- BS.readFile filepath
  return (decode contents :: Maybe PointSelection)

loadPointsComplex :: String -> IO (Maybe PointSelection)
loadPointsComplex filepath = do
  contents <- readFile filepath
  let zs = concatMap g $ lines contents
        where g = maybeToList . readMaybe :: String -> [Complex Double]
      ps = map f zs
        where f (x :+ y) = BBPoint x y 1
  return $ if null ps
           then Nothing
           else Just (emptyPS { pointList = ps })

render :: BBrotConf -> IO ()
render conf = do
  -- Load points of interest, render their orbits into a PNG image.
  whenNormal $ putStrLn $ "Loading cache " ++ icachepath conf ++ " ..."
  maybePS <- if isComplex conf
             then loadPointsComplex $ icachepath conf
             else loadPointsJson $ icachepath conf
  when (isNothing maybePS) $ do
    hPutStrLn stderr $ "failed to parse " ++ icachepath conf
    unless (isComplex conf) $
      hPutStrLn stderr "try with flag -z"
    exitFailure

  let psel = pointList $ fromMaybe emptyPS maybePS
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
  whenNormal $ putStrLn "done plotting"

  img2 <- freeze img :: IO (UArray (Int, Int) Word32)
  let values = elems img2
      !v0 = head values
      (!total, !smallest, !biggest) = foldl' f (0, v0, v0) values
        where f (!x, !y, !z) !a = (a + x, min a y, max a z)

  whenNormal $ do
    putStrLn $ "img points: " ++ show total
    putStrLn $ "value range: " ++ show smallest ++ "-" ++ show biggest

  let outfile = fromMaybe defPath (imagepath conf)
        where defPath = icachepath conf ++ ".png"
  whenNormal $ putStrLn $ "Writing " ++ outfile ++ " ..."
  let colorScheme = case palette conf of
        Flames -> flames
        Gray -> gray
        Reddish -> reddish
      curveFunc = case curve conf of
        Line -> id
        Root -> sqrt
        Square -> (**2)
      pixFunc = toPixel curveFunc smallest biggest colorScheme
      renderer i j = pixFunc $ img2!(i,j)
  writePng outfile $ generateImage (flip $ renderer) yres xres


showCells :: BBrotConf -> IO ()
showCells conf = do
  -- Compute selection of cells (squares in the complex plane) that
  -- are close to the edge of the Mandelbrot set, and render them as
  -- animated GIF.
  let step = gridStep conf
      bailout = maxIters conf
      xres = 1000
      yres = 1000
      !cells = selectCells step bailout
  whenNormal $ putStrLn $ "cell count: " ++ show (length cells)

  let red   = PixelRGB8 255 0 0
      black = PixelRGB8 0 0 0
      grey  = PixelRGB8 64 64 64

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
    forM_ coords $ \(i, j) ->
      writeArray cellMap (i, j) True

  whenNormal $ putStrLn "rendering mandel"
  let imgMandel = generateImage (flip $ mandelRenderer) yres xres
      inMandelbrotSet z = inSet 0 bailout z
      mandelRenderer i j = if inMandelbrotSet $ toPlaneCoords xres yres i j
                           then grey else black

  cellMapPure <- freeze cellMap :: IO (UArray (Int, Int) Bool)
  let imgCells = generateImage (flip $ cellRenderer) yres xres
      cellRenderer i j = if cellMapPure!(i,j)
                         then red else mandelRenderer i j

  whenNormal $ putStrLn $ "writing " ++ animpath conf ++ " ..."
  case writeGifAnimation (animpath conf) 100 LoopingForever [imgMandel, imgCells] of
    Left err -> putStrLn $ "error generating gif animation: " ++ err
    Right action -> action
