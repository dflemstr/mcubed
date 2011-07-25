module Data.CubeTree where

import Data.Word (Word)
import Data.Bits
import Data.Coord (Coord)
import qualified Data.Coord as Coord

data CubeTree a =
  Empty |
  Leaf { value :: a } |
  Node { value :: a
       , c000 :: CubeTree a
       , c001 :: CubeTree a
       , c010 :: CubeTree a
       , c011 :: CubeTree a
       , c100 :: CubeTree a
       , c101 :: CubeTree a
       , c110 :: CubeTree a
       , c111 :: CubeTree a
       }
  deriving (Eq)

data Octant =
  C000 |
  C001 |
  C010 |
  C011 |
  C100 |
  C101 |
  C110 |
  C111
  deriving (Eq, Ord, Bounded, Enum)

type Octants = [Octant]

type Height = Word

empty :: CubeTree a
empty = Empty

emptyNode   :: a -> CubeTree a
emptyNode a = Node a Empty Empty Empty Empty Empty Empty Empty Empty

fullNode   :: a -> CubeTree a
fullNode a = split $ Leaf a

split              :: CubeTree a -> CubeTree a
split l @ (Leaf a) = Node a l l l l l l l l
split tree         = tree

selectOctant                   :: Bool -> Bool -> Bool -> Octant
selectOctant False False False = C000
selectOctant False False True  = C001
selectOctant False True  False = C010
selectOctant False True  True  = C011
selectOctant True  False False = C100
selectOctant True  False True  = C101
selectOctant True  True  False = C110
selectOctant True  True  True  = C111

takeOctant          :: Octant -> CubeTree a -> Maybe (CubeTree a)
takeOctant oct tree =
  case tree of
    Empty  -> Nothing
    Leaf{} -> Nothing
    n      -> Just $
      case oct of
        C000 -> c000 n
        C001 -> c001 n
        C010 -> c010 n
        C011 -> c011 n
        C100 -> c100 n
        C101 -> c101 n
        C110 -> c110 n
        C111 -> c111 n

mapBranches        :: (CubeTree a -> CubeTree a)
                      -> CubeTree a -> CubeTree a
mapBranches f node =
  node { c000 = f . c000 $ node
       , c001 = f . c001 $ node
       , c010 = f . c010 $ node
       , c011 = f . c011 $ node
       , c100 = f . c100 $ node
       , c101 = f . c101 $ node
       , c110 = f . c110 $ node
       , c111 = f . c111 $ node }

mapOctant            :: Octant -> (CubeTree a -> CubeTree a) ->
                        CubeTree a -> CubeTree a
mapOctant oct f tree =
  case tree of
    Empty  -> tree
    Leaf{} -> tree
    n @ (Node _ oc000 oc001 oc010 oc011 oc100 oc101 oc110 oc111) ->
      case oct of
        C000 -> n { c000 = f oc000 }
        C001 -> n { c001 = f oc001 }
        C010 -> n { c010 = f oc010 }
        C011 -> n { c011 = f oc011 }
        C100 -> n { c100 = f oc100 }
        C101 -> n { c101 = f oc101 }
        C110 -> n { c110 = f oc110 }
        C111 -> n { c111 = f oc111 }

coordToOctants     :: Height -> Coord -> Octants
coordToOctants 0 _ = []
coordToOctants n c =
  newOctant : coordToOctants newPos c
  where
    test f = testBit (f c) (fromIntegral newPos)
    newPos = n - 1
    newOctant =
      selectOctant
      (test Coord.x)
      (test Coord.y)
      (test Coord.z)

insertVia                 :: Octants -> a -> CubeTree a -> CubeTree a
insertVia []       a _    = Leaf a
insertVia (o : os) a tree =
  case tree of -- TODO this can be done with some sort of fold
    Empty  -> mapOctant o (insertVia os a) empty
    Leaf a -> mapOctant o (insertVia os a) $ fullNode a
    Node{} -> mapOctant o (insertVia os a) tree

insertAt   :: Height -> Coord -> a -> CubeTree a -> CubeTree a
insertAt h = insertVia . coordToOctants h

narrowVia               :: Octants -> CubeTree a -> Maybe (CubeTree a)
narrowVia []       tree = Just tree
narrowVia (o : os) tree =
  case tree of
    Empty  -> Nothing
    Leaf{} -> Nothing
    Node{} -> narrowVia os t
      where
        Just t = takeOctant o tree

getVia         :: Octants -> CubeTree a -> Maybe a
getVia os tree =
  case narrowVia os tree of
    Just (Leaf a) -> Just a
    _             -> Nothing

getAt   :: Height -> Coord -> CubeTree a -> Maybe a
getAt h = getVia . coordToOctants h
