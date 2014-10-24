import Data.Sequence as S
import Data.List
import Data.Foldable
import System.IO
import Prelude as P
import qualified Data.Text as T

data Vertex a = Vertex a a a
    deriving Read
instance Show a => Show (Vertex a) where
    show  = T.unpack . vertToText

vertToText :: (Show a) => Vertex a -> T.Text
vertToText (Vertex vx vy vz) = T.intercalate (T.singleton ' ') 
                               [T.singleton 'v', T.pack $ show vx, T.pack $ show vy, T.pack $ show vz]

data Triangle = Triangle Int Int Int
    deriving Read
instance Show Triangle where
    show  = T.unpack . triToText

triToText :: Triangle -> T.Text
triToText (Triangle v0 v1 v2) = T.intercalate (T.singleton ' ') 
                               [T.singleton 'f', T.pack $ show v0, T.pack $ show v1, T.pack $ show v2]

data Mesh a = Mesh (Seq (Vertex a)) (Seq Triangle)
    deriving (Read)
instance Show a => Show (Mesh a) where
    show = T.unpack . meshToText

meshToText :: (Show a) => Mesh a -> T.Text
meshToText (Mesh verts tris) = T.intercalate (T.pack "\n") 
                               (toList (fmap vertToText verts >< fmap triToText tris))

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

radialGridMultipleMesh :: Int -> [Mesh Double]
radialGridMultipleMesh axisResolution = meshList
    where 
        meshList :: [Mesh Double]
        meshList = map (\tris -> Mesh verts tris) trisList

        radiusRange :: [Double]
        radiusRange = map (\t -> t / (fromIntegral (axisResolution - 1))) (P.take axisResolution [1.0, 2.0 ..])

        thetaRange :: [Double]
        thetaRange = map (\t -> 2.0 * pi * t / (fromIntegral (axisResolution - 1))) (P.take axisResolution [0.0, 1.0 ..])

        trisLong = (fromList $ P.concatMap takeOffSetGiveTris $ allPairs axisResolution) >< --all but center
                   fromList [Triangle (S.length verts) r1 r0 | (r0, r1) <- firstRowPairs axisResolution] --center tris

        trisList = recursiveSplitAt 65534 trisLong

        verts = (fromList [Vertex (radius * cos theta) 0.0 (radius * sin theta) | radius <- radiusRange, theta <- thetaRange]) |> Vertex 0.0 0.0 0.0

recursiveSplitAt :: Int -> Seq a -> [Seq a]
recursiveSplitAt splitAt inSeq | S.length inSeq <= splitAt = [inSeq]
recursiveSplitAt splitAt inSeq | otherwise = firstSeq : recursiveSplitAt splitAt secSeq
    where
        (firstSeq, secSeq) = S.splitAt splitAt inSeq

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
    let meshList = radialGridMultipleMesh 250 :: [Mesh Double]
    print $ P.length meshList
    --meshToObj (squareGrid 225) "squareGrid225.obj"
    P.mapM_ (\(index, mesh) -> meshToObj mesh ("radialGrid250_" ++ show index ++ ".obj")) (P.zip [0, 1..] meshList)
    --meshToObj (radialGrid 250) "radialGrid250.obj"
    --print (pairs [1,2,3])

