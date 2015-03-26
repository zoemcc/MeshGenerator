module FractalSurface where

import Data.Sequence as S
import Data.List
import Data.Foldable
import System.IO
import Prelude as P
import qualified Data.Text as T
import Types
import Linear
import Control.Lens
import Debug.Trace

mandelBulb :: (Ord a, Floating a, Show a) => Int -> a -> V3 a -> (V3 a -> a)
--mandelBulb numCutoff n c = P.undefined
mandelBulb numCutoff n c = cutoffFunc
    where
        --cutoffFunc :: (Floating a) => Int -> (V3 a -> a)
        cutoffFunc inputPoint = cutoff $ iterationPoint inputPoint

        --cutoff :: Int -> V3 a -> a
        cutoff currentPoint = if (norm currentPoint > 20.0) then 1.0 else (-1.0)
        --cutoff currentPoint = Debug.Trace.trace (show currentPoint) $ (if (norm currentPoint > 20.0) then 1.0 else (-1.0))

        --iterationPoint :: (Floating a) => V3 a -> V3 a
        iterationPoint inputPoint = mandelBulbHelper numCutoff n c inputPoint

--mandelBulbHelper :: (Ord a, Floating a) => Int -> a -> V3 a -> V3 a -> V3 a
--mandelBulbHelper numLeft n c currentPoint | n > 0 = mandelBulbHelper (numLeft - 1) n c (nthPower n currentPoint + c)
--mandelBulbHelper numLeft n c currentPoint | otherwise = currentPoint

mandelBulbHelper :: (Ord a, Floating a, Show a) => Int -> a -> V3 a -> V3 a -> V3 a
mandelBulbHelper numLeft n c currentPoint | n > 0 = P.last $ P.take (numLeft + 1) $ P.iterate iterateOnce currentPoint
    where
        iterateOnce currentPoint = nthPower n currentPoint + c
mandelBulbHelper numLeft n c currentPoint | otherwise = currentPoint



nthPower :: (Floating a, Show a) => a -> V3 a -> V3 a
--nthPower n currentPoint = Debug.Trace.trace (show newPoint) $ newPoint
nthPower n currentPoint = newPoint
    where
        newPoint = (r ** n) *^ (V3 newX newY newZ)
        newX = P.sin (n * theta) * P.cos (n * theta)
        newY = P.sin (n * theta) * P.sin (n * theta)
        newZ = P.cos (n * theta)
        r = norm currentPoint
        phi = P.atan ((currentPoint ^._y) / (currentPoint ^._x))
        theta = P.acos ((currentPoint ^._z) / r)


