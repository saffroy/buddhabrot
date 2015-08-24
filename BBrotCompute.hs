{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module BBrotCompute(compute) where

import Data.Complex
import System.Random
import Text.Printf
import System.Console.CmdArgs

import BBrotConf


instance Random (Complex Double) where
  randomR (!loPoint, !hiPoint) g = (r :+ i, g2)
    where (r, g1) = randomR (realPart loPoint, realPart hiPoint) g
          (i, g2) = randomR (imagPart loPoint, imagPart hiPoint) g1
  random = randomR (0.0 :+ 0.0, 1.0 :+ 1.0)


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

toUnit n | n < 10^3  = show n ++ ""
         | n < 10^6  = show (n `div` 10^3) ++ "K"
         | n < 10^9  = show (n `div` 10^6) ++ "M"
         | otherwise = show (n `div` 10^9) ++ "G"

compute conf = do
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
