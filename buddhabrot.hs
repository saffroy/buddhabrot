{- buddhabrot reimplementation
   based on C source at: http://paulbourke.net/fractals/buddhabrot/
   see also:
   http://www.superliminal.com/fractals/bbrot/bbrot.htm
   http://erleuchtet.org/2010/07/ridiculously-large-buddhabrot.html
   http://www.steckles.com/buddha/
   http://softologyblog.wordpress.com/2011/06/26/buddhabrot-fractals/
   http://kindofdoon.blogspot.fr/2012/09/the-colored-orbit-buddhabrot.html
-}

import System.Random
import Data.Array.IO
import Data.Word(Word32, Word8)
import qualified  Data.ByteString.Lazy as L
import Text.Printf

data CPoint = CPoint { re :: Double, im :: Double }
              deriving (Eq, Show)
instance Random CPoint where
  randomR (loPoint, hiPoint) g = (CPoint { re = r, im = i}, g2)
    where (r, g1) = randomR (re loPoint, re hiPoint) g
          (i, g2) = randomR (im loPoint, im hiPoint) g1
  random = randomR (CPoint { re = 0, im = 0 }, CPoint { re = 1, im = 1 })

samplesMillions = 500
samples = samplesMillions * 1000 * 1000

minK =  1 * 1000
maxK = 20 * 1000
radius = 4

imgpath =
  printf "/tmp/buddhabrot-%dM-%dK_%dK_id.tga"
    (div samples 1000000) (div minK 1000) (div maxK 1000)

xpixels = 1000 :: Int
ypixels = 1000 :: Int
pixels = xpixels * ypixels

loCorner = CPoint { re = -2, im = -1.5 }
hiCorner = CPoint { re =  1, im =  1.5 }

colorScheme = flames
normFunc = id -- or sqrt or (**2)

--

xmin = re loCorner
xmax = re hiCorner
ymin = im loCorner
ymax = im hiCorner

xrange = (xmin, xmax)
yrange = (ymin, ymax)


rel :: (Double, Double) -> Double -> Double
rel (lo, hi) v = (v - lo) / (hi - lo)

toPix :: Int -> (Double, Double) -> Double -> Int
toPix pixrange realrange a = floor $ (fromIntegral pixrange) * (rel realrange a)
                              
toWindow :: CPoint -> (Int, Int)
toWindow pt = (toPix xpixels xrange $ re pt, toPix ypixels yrange $ im pt)

inWindow :: CPoint -> Bool
inWindow CPoint { re = x, im = y } = x >= xmin && x < xmax && y >= ymin && y < ymax

iterations :: Double -> Double -> Double -> Double -> Int -> Int
iterations x y x0 y0 k  =
  let x2 = x * x
      y2 = y * y
  in
   if k == maxK || x2 + y2 > radius
   then k
   else iterations (x2 - y2 + x0) (2 * x * y + y0) x0 y0 (k + 1)

inSet :: CPoint -> Bool
inSet pt = k >= minK && k < maxK
  where k = iterations (re pt) (im pt) (re pt) (im pt) 0

orbit :: Double -> Double -> Double -> Double -> [CPoint] -> [CPoint]
orbit x y x0 y0 l =
  let x2 = x * x
      y2 = y * y
      newx = (x2 - y2 + x0)
      newy = (2 * x * y + y0)
      pt = CPoint { re = newx, im = newy }
  in
   if x2 + y2 > radius
   then l
   else orbit newx newy x0 y0 (pt : l)

orbs :: CPoint -> [CPoint]
orbs pt = orbit (re pt) (im pt) (re pt) (im pt) []

plotPix :: IOUArray Int Word32 -> (Int, Int) -> IO ()
plotPix img (x, y) = do
  let offset = y * xpixels + x
  v <- readArray img offset
  writeArray img offset (v + 1)

norm :: Word32 -> Word32 -> Word32 -> Word8
norm min max cnt = fromIntegral v
  where s = 2.0 * fromIntegral (cnt - min) / fromIntegral (max - min)
        t = if s > 1.0 then 1.0 else s
        -- u = t ** 2
        u = normFunc t
        v = floor $ u * 255

triple :: Word8 -> [Word8]
triple x = [x, x, x]

reddish :: Word8 -> [Word8]
reddish x = [0, 2 * g, 2 * r]
  where r = min x 127
        g = min (x - r) 127

flames :: Word8 -> [Word8]
flames x = [b, g, r]
  where xr = min 120 x
        xg = min 120 (x - xr)
        xb = min 15 (x - xr - xg)
        r = xr * 2 + xr `div` 10
        g = xg * 2 + xg `div` 10
        b = xb * 17

toTga :: [Word32] -> L.ByteString
toTga values = L.pack (header ++ body)
  where header = map fromIntegral [
          0, -- length of ID
          0, -- no color map
          2, -- uncompressed RGB
          0, -- index of color map entry
          0,
          0, -- color map length
          0,
          0, -- color map size
          0, -- X origin
          0,
          0, -- Y origin
          0,
          xpixels `mod` 256, -- X width
          xpixels `div` 256,
          ypixels `mod` 256, -- Y width
          ypixels `div` 256,
          24, -- 24 bit bitmap
          0
          ]
        biggest = maximum values
        smallest = minimum values
        body = concat $ map (colorScheme . norm smallest biggest) values

main = do
  putStrLn $ printf "Sampling %d millions points..." samplesMillions
  let seed = mkStdGen 0
  -- seed <- getStdGen
  let points = take samples $ randomRs (loCorner, hiCorner) seed
      selected = filter inSet points
      res = filter inWindow $ concat $ map orbs selected
  let coords = map toWindow res
  
  img <- newArray (0, pixels - 1) (0 :: Word32) :: IO (IOUArray Int Word32)
  sequence_ $ map (plotPix img) coords
  putStrLn $ "Writing " ++ imgpath ++ " ..."
  values <- getElems img
  L.writeFile imgpath $ toTga values
  putStrLn "Done!"
