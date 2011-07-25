module Data.Coord where

import Data.Word (Word64)
import Data.Bits

data Coord =
  Coord { x :: Scalar
        , y :: Scalar
        , z :: Scalar
        }
  deriving (Show, Eq)

type Scalar = Word64

truncateFrom i c =
  Coord (trunc $ x c) (trunc $ y c) (trunc $ z c)
  where
    trunc s = s .&. mask
    mask = complement 0 `shiftL` i