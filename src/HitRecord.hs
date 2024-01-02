module HitRecord where

import Vector

data HitRecord = HitRecord
  { point :: Vec3
  , normal :: Vec3
  , t :: Float
  , frontFace :: Bool
  }
