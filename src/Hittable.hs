{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hittable where

import Vector
import Ray

data HitRecord = HitRecord
  { point :: Vec3
  , normal :: Vec3
  , t :: Float
  , frontFace :: Bool
  }
  
class Hittable a where
  hit :: a -> Ray -> Float -> Float -> Maybe HitRecord
  

data Sphere = Sphere
  { center :: Vec3
  , radius :: Float
  }
  deriving (Show)

instance Hittable Sphere where
  hit sphere ray rayMin rayMax =
    let oc = ray.origin - sphere.center
        a = dot ray.direction ray.direction
        b = dot oc ray.direction
        c = (dot oc oc) - sphere.radius*sphere.radius
        t' = getNearestRoot a b c rayMax rayMin
    in
      case t' of
        Just t -> let point = rayAt ray t
                      outwardNormal = (point - sphere.center) ^/ sphere.radius
                      frontFace = dot ray.direction outwardNormal < 0
                      normal = if frontFace then outwardNormal else -outwardNormal
                  in Just $ HitRecord point normal t frontFace
        Nothing -> Nothing

    where

      getNearestRoot :: Float -> Float -> Float -> Float -> Float -> Maybe Float
      getNearestRoot a b c rayMax rayMin =
        do
          discriminant <- let v = b*b - a*c in if v < 0 then Nothing else return v

          let sqrtd = sqrt discriminant
          let root = (-b - (sqrtd) ) / a

          if root > rayMin && root < rayMax
            then Just root
            else let root = (-b + (sqrtd) ) / a
                 in
                   if root > rayMin && root < rayMax
                   then Just root
                   else Nothing

      
