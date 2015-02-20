module FractalSurface where

import Data.Sequence as S
import Data.List
import Data.Foldable
import System.IO
import Prelude as P
import qualified Data.Text as T
import Types
import Linear

mandelBulb :: (Floating a) => Int -> Int -> V3 a -> (V3 a -> a)
--mandelBulb numCutoff n c = P.undefined
mandelBulb numCutoff n c = cutoffFunc numCutoff
    where
        cutoffFunc :: (Floating a) => Int -> (V3 a -> a)
        cutoffFunc numCutoff inputPoint = cutoff numCutoff $ iterationPoint inputPoint

        cutoff = P.undefined


        iterationPoint :: (Floating a) => V3 a -> V3 a
        iterationPoint inputPoint = mandelBulbHelper numCutoff n c inputPoint

mandelBulbHelper :: (Floating a) => Int -> Int -> V3 a -> V3 a
mandelBulbHelper numLeft n c currentPoint | n > 0 = mandelBulbHelper (numLeft - 1) n c (nthPower n currentPoint + c)
mandelBulbHelper numLeft n c currentPoint | otherwise = currentPoint

nthPower n currentPoint = P.undefined


