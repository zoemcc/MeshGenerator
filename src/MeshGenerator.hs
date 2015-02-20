module MeshGenerator  where

import Data.Sequence as S
import Data.List
import Data.Foldable
import System.IO
import Prelude as P
import qualified Data.Text as T
import Types


testVerts :: Seq (Vertex Double)
testVerts = fromList [Vertex 0.0 0.0 0.0, Vertex 0.0 0.0 1.0, Vertex 1.0 0.0 1.0, Vertex 1.0 0.0 0.0]

testTris :: Seq Triangle
testTris = fromList [Triangle 1 2 3, Triangle 1 3 4]

testSquare :: Mesh Double
testSquare = Mesh testVerts testTris

squareGrid :: Int -> Mesh Double
squareGrid axisResolution = Mesh verts tris
    where 

        axisRange :: [Double]
        axisRange = map (\t -> 2.0 * (t - 0.5 * fromIntegral (axisResolution - 1)) / (fromIntegral (axisResolution - 1))) (P.take axisResolution [0.0, 1.0 ..])

        tris = fromList $ P.concatMap takeOffSetGiveTris $ allPairs axisResolution
        verts = fromList [Vertex vx 0.0 vz | vx <- axisRange, vz <- axisRange]

radialGrid :: Int -> Mesh Double
radialGrid axisResolution = Mesh verts tris
    where 

        radiusRange :: [Double]
        radiusRange = map (\t -> t / (fromIntegral (axisResolution - 1))) (P.take axisResolution [1.0, 2.0 ..])

        thetaRange :: [Double]
        thetaRange = map (\t -> 2.0 * pi * t / (fromIntegral (axisResolution - 1))) (P.take axisResolution [0.0, 1.0 ..])

        tris = (fromList $ P.concatMap takeOffSetGiveTris $ allPairs axisResolution) >< --all but center
                fromList [Triangle (S.length verts) r1 r0 | (r0, r1) <- firstRowPairs axisResolution] --center tris
        verts = (fromList [Vertex (radius * cos theta) 0.0 (radius * sin theta) | radius <- radiusRange, theta <- thetaRange]) |> Vertex 0.0 0.0 0.0

recursiveSplitAt :: Int -> Seq a -> [Seq a]
recursiveSplitAt splitAt inSeq | S.length inSeq <= splitAt = [inSeq]
recursiveSplitAt splitAt inSeq | otherwise = firstSeq : recursiveSplitAt splitAt secSeq
    where
        (firstSeq, secSeq) = S.splitAt splitAt inSeq

splitMeshAt :: Int -> Mesh a -> [Mesh a]
splitMeshAt splitAt (Mesh verts trisLong) = meshList
    where
        meshList = map (\tris -> Mesh verts tris) trisList
        trisList = recursiveSplitAt splitAt trisLong

takeOffSetGiveTris :: ((Int, Int), (Int, Int)) -> [Triangle]
takeOffSetGiveTris ((r0, r1), (c0, c1)) = [(Triangle (r0 + c0) (r1 + c0) (r0 + c1)), (Triangle (r0 + c1) (r1 + c0) (r1 + c1))]

firstRowPairs :: Int -> [(Int, Int)]
firstRowPairs resolution = pairs $ P.take resolution [1, 2 ..]

colPairs :: Int -> [(Int, Int)]
colPairs resolution      = pairs $ P.take resolution [0, resolution ..]

allPairs :: Int -> [((Int, Int), (Int, Int))]
allPairs resolution      = [(rowOffset, colOffset) | colOffset <- colPairs resolution, rowOffset <- firstRowPairs resolution]

pairs :: [a] -> [(a, a)]
pairs xs@(y:ys) = P.zip xs ys
pairs _         = []

meshToObj :: (Show a) => Mesh a -> String -> IO ()
meshToObj mesh filename = writeFile filename (show mesh ++ "\n")

meshVertLength :: Mesh a -> Int
meshVertLength (Mesh verts tris) = S.length verts

meshTrisLength :: Mesh a -> Int
meshTrisLength (Mesh verts tris) = S.length tris

