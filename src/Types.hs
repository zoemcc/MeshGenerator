{-# LANGUAGE ScopedTypeVariables #-}
module Types where

import Data.Sequence as S
import Data.List
import Data.Foldable
import System.IO
import Prelude as P
import qualified Data.Text as T
import Data.Monoid
import Data.Vector
import Linear
import Control.Lens

data Vertex a = Vertex a a a
    deriving Read
instance Show a => Show (Vertex a) where
    show  = T.unpack . vertToText

linearToVert :: V3 a -> Vertex a 
linearToVert vector = Vertex (vector ^._x) (vector ^._y) (vector ^._z)

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

data IndependentTriangle a = IndependentTriangle (V3 a) (V3 a) (V3 a)
    deriving (Read, Show)

independentTriangleToSeqVertices :: IndependentTriangle a -> Seq (Vertex a)
independentTriangleToSeqVertices (IndependentTriangle v0 v1 v2) = S.fromList (P.map linearToVert [v0, v1, v2])

data GridCell a = GridCell (Vector (V3 a)) (Vector a) -- grid points and levels
    deriving (Read, Show)

data IndependentTriangleMesh a = IndependentTriangleMesh (Seq (IndependentTriangle a))
    deriving (Read, Show)

instance Monoid (IndependentTriangleMesh a) where
    mempty = IndependentTriangleMesh (S.empty)
    (IndependentTriangleMesh mesh0) `mappend` (IndependentTriangleMesh mesh1) = IndependentTriangleMesh (mesh0 >< mesh1)

independentTriangleMeshToMeshUnCompressed :: forall a. IndependentTriangleMesh a -> Mesh a
independentTriangleMeshToMeshUnCompressed (IndependentTriangleMesh independentTriangles) = Mesh vertices triangles
    where
        vertices :: Seq (Vertex a)
        vertices = foldMap independentTriangleToSeqVertices independentTriangles

        triangles :: Seq Triangle
        triangles = mapWithIndex triangleToUncompressedIndices independentTriangles

        triangleToUncompressedIndices :: Int -> IndependentTriangle a -> Triangle
        triangleToUncompressedIndices index _ = Triangle (3 * index) (3 * index + 1) (3 * index + 2)

data Mesh a = Mesh (Seq (Vertex a)) (Seq Triangle)
    deriving (Read)
instance Show a => Show (Mesh a) where
    show = T.unpack . meshToText

meshToText :: (Show a) => Mesh a -> T.Text
meshToText (Mesh verts tris) = T.intercalate (T.pack "\n") 
                               (Data.Foldable.toList (fmap vertToText verts >< fmap triToText tris))

data Tetrahedron = Tetrahedron Int Int Int Int
    deriving Read
--instance Show Tetrahedron where
    --show  = T.unpack . triToText

data TetMesh a = TetMesh (Seq (Vertex a)) (Seq Tetrahedron)

type ImplicitSurface a = V3 a -> a


toV3fromList :: [a] -> Maybe (V3 a)
toV3fromList (x:y:z:[]) = Just (V3 x y z)
toV3fromList _          = Nothing


