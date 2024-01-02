{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Ray where

import Vector

data Ray = Ray
  { origin :: !Vec3
  , direction :: !Vec3
  }

rayAt :: Ray -> Float -> Vec3
rayAt ray t = ray.origin + (t *^ ray.direction)
