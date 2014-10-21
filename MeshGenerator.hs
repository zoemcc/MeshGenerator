import Data.Sequence as S
import Data.List
import Data.Foldable
import System.IO
import Prelude as P

data Vertex a = Vertex a a a
    deriving Read
instance Show a => Show (Vertex a) where
    show (Vertex vx vy vz) = intercalate " " ["v", show vx, show vy, show vz]

data Triangle = Triangle Int Int Int
    deriving Read
instance Show Triangle where
    show (Triangle v0 v1 v2) = intercalate " " ["f", show v0, show v1, show v2]

data Mesh a = Mesh (Seq (Vertex a)) (Seq Triangle)
    deriving (Read)
instance Show a => Show (Mesh a) where
    show (Mesh verts tris) = intercalate "\n" (toList (fmap show verts >< fmap show tris))

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

main :: IO ()
main = do 
    meshToObj testSquare "testSquare.obj"
    print testSquare
    print $ squareGrid 3
    print $ radialGrid 4
    --meshToObj (squareGrid 225) "squareGrid225.obj"
    meshToObj (radialGrid 250) "radialGrid250.obj"
    --print (pairs [1,2,3])

