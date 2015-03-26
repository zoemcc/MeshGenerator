module Main  where

import Data.Sequence as S
import Data.List
import Data.Foldable
import System.IO
import Prelude as P
import qualified Data.Text as T
import Control.Lens
import Types
import MeshGenerator
import Linear
import FractalSurface
import MarchingCubes

sphere :: V3 Double -> Double
sphere v = norm v - 1.0


boxAxis :: Double -> V3 Double -> Double
boxAxis lim v | maxAxis v > lim && minAxis v < (-lim) = max (maxAxis v - lim) (-(minAxis v + lim))
boxAxis lim v | minAxis v < (-lim) = (-(minAxis v + lim))
boxAxis lim v | maxAxis v > lim = maxAxis v - lim
boxAxis lim v | otherwise = max (maxAxis v - lim) (-(minAxis v + lim))

minAxis :: V3 Double -> Double
minAxis v = min (min (v ^._x) (v ^._y)) (v ^._z)

maxAxis :: V3 Double -> Double
maxAxis v = max (max (v ^._x) (v ^._y)) (v ^._z)

mandelBulb8 :: V3 Double -> Double
--mandelBulb8 = mandelBulb 7 3.0 (V3 (-0.20) (-0.80) (-0.4)) 
mandelBulb8 = mandelBulb 7 9.0 (V3 (-0.9) (0.1) (-0.2)) --many saturn discs
--mandelBulb8 = mandelBulb 15 2.0 (V3 0.7 (-0.7) 0.45) --chalice

main :: IO ()
main = do 
    --meshToObj testSquare "testSquare.obj"
    --print testSquare
    --print $ squareGrid 3
    --print $ radialGrid 4

    let maxTriNum = 65534

    {-
    let meshListRadial = splitMeshAt maxTriNum (radialGrid 250) :: [Mesh Double]
    print $ P.length meshListRadial
    print $ "Radial number of vertices: "  ++ show (P.map meshVertLength meshListRadial)
    print $ "Radial number of triangles: " ++ show (P.map meshTrisLength meshListRadial)
    P.mapM_ (\(index, mesh) -> meshToObj mesh ("radialGrid250_" ++ show index ++ ".obj")) (P.zip [0, 1..] meshListRadial)

    let meshListSquare = splitMeshAt maxTriNum (squareGrid 500) :: [Mesh Double]
    print $ P.length meshListSquare
    print $ "Square number of vertices: "  ++ show (P.map meshVertLength meshListSquare)
    print $ "Square number of triangles: " ++ show (P.map meshTrisLength meshListSquare)
    P.mapM_ (\(index, mesh) -> meshToObj mesh ("squareGrid500_" ++ show index ++ ".obj")) (P.zip [0, 1..] meshListSquare)
    -}

    let minNum = -2.0
    let maxNum = 2.0
    let minCorner = V3 minNum minNum minNum :: V3 Double
    let maxCorner = V3 maxNum maxNum maxNum :: V3 Double
    --let sphereIndepMesh = marchingCubesGrid sphere 0.0 200 minCorner maxCorner
    --let sphereIndepMesh = marchingCubesGrid sphere 0.0 50 minCorner maxCorner
    --let sphereMesh = independentTriangleMeshToMeshUnCompressed sphereIndepMesh
    --let meshListSphere = splitMeshAt maxTriNum sphereMesh :: [Mesh Double]
    --P.mapM_ (\(index, mesh) -> meshToObj mesh ("sphereMesh_" ++ show index ++ ".obj")) (P.zip [0, 1..] meshListSphere)
    --meshToObj sphereMesh "sphereMesh.obj"

    --let boxIndepMesh = marchingCubesGrid (boxAxis 0.8) 0.0 50 minCorner maxCorner
    --let boxMesh = independentTriangleMeshToMeshUnCompressed boxIndepMesh
    --meshToObj boxMesh "boxMesh.obj"

    let mandelIndepMesh = marchingCubesGrid mandelBulb8 0.0 200 minCorner maxCorner
    let mandelMesh = independentTriangleMeshToMeshUnCompressed mandelIndepMesh
    meshToObj mandelMesh "mandel8Mesh.obj"

