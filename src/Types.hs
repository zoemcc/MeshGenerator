module Types where

import Data.Sequence as S
import Data.List
import Data.Foldable
import System.IO
import Prelude as P
import qualified Data.Text as T
import Data.Vector
import Linear

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


data IndependentTriangle a = IndependentTriangle (V3 a) (V3 a) (V3 a)
    deriving (Read, Show)

data GridCell a = GridCell (Vector (V3 a)) (Vector a)
    deriving (Read, Show)


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

--type ImplicitSurface a = V3 a -> a



