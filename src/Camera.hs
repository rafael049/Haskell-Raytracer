module Camera where

import Vector

data Camera = Camera
  { position :: Vec3
  , front :: Vec3
  , focalLength :: Float
  }
