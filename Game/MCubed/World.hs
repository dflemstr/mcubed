module Game.MCubed.World where

import Control.Arrow
import Control.Monad
import Data.Lens.Lazy
import qualified Data.Maybe as Maybe
import Data.CubeTree (CubeTree)
import qualified Data.CubeTree as CubeTree
import Data.Coord (Coord)
import qualified Data.Coord as Coord

data World =
  World { getSize :: CubeTree.Height
        , getBlockTree :: CubeTree Block
        }
  deriving (Eq)

type Depth = CubeTree.Height

type Generator = Coord -> Block

setBlockTree            :: CubeTree Block -> World -> World
setBlockTree tree world = world { getBlockTree = tree }

blockTree :: Lens World (CubeTree Block)
blockTree = lens getBlockTree setBlockTree

getBlock             :: Coord -> World -> Maybe Block
getBlock coord world =
  CubeTree.getAt height coord $ getBlockTree world
  where
    height = getSize world

setBlock                   :: Coord -> Block -> World -> World
setBlock coord block world =
  (blockTree ^%= CubeTree.insertAt height coord block) world
  where
    height = getSize world

getLoadedBlock             :: Coord -> World -> Block
getLoadedBlock coord world =
  Maybe.fromMaybe err loadedBlk
  where
    err = error $ "getLoadedBlock: No block at " ++ show coord
    loadedBlk = getBlock coord world

setLoadedBlock :: Coord -> Block -> World -> World
setLoadedBlock = setBlock

loadedBlock :: Coord -> Lens World Block
loadedBlock = uncurry lens . (getLoadedBlock &&& setLoadedBlock)

loadedBlockAt       :: Coord.Scalar -> Coord.Scalar -> Coord.Scalar
                       -> Lens World Block
loadedBlockAt x y z = loadedBlock $ Coord.Coord x y z

getBlockGen                 :: Generator -> Coord -> World -> Block
getBlockGen gen coord world =
  Maybe.fromMaybe (gen coord) (getBlock coord world)

setBlockGen :: Generator -> Coord -> Block -> World -> World
setBlockGen = const setBlock

blockGen     :: Generator -> Coord -> Lens World Block
blockGen gen = uncurry lens . (getBlockGen gen &&& setBlockGen gen)

generateDepth :: Generator -> Depth
                 -> CubeTree Block -> CubeTree Block
generateDepth = undefined -- stub

generate :: Generator -> CubeTree.Height
            -> Depth -> Depth -> Coord
            -> CubeTree Block -> CubeTree Block
generate = undefined -- stub

data Block = Block deriving (Eq) -- stub