module Main  where

import Data.Sequence as S
import Data.List
import Data.Foldable
import System.IO
import Prelude as P
import qualified Data.Text as T
import Types
import MeshGenerator
import Linear
--import FractalSurface
import MarchingCubes

sphere :: V3 Double -> Double
sphere v = norm v - 1.0

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

    let minCorner = V3 (-1.5) (-1.5) (-1.5) :: V3 Double
    let maxCorner = V3   1.5    1.5    1.5  :: V3 Double
    let sphereIndepMesh = marchingCubesGrid sphere 0.0 20 minCorner maxCorner
    let sphereMesh = independentTriangleMeshToMeshUnCompressed sphereIndepMesh
    meshToObj sphereMesh "sphereMesh.obj"

