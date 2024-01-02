{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Material where

import System.Random
import Debug.Trace

import Ray
import HitRecord
import Vector

data MaterialType = Lambertian
                  | Metal
                  deriving (Show)

data Material = Material
  { albedo :: Vec3
  , materialType :: MaterialType
  }
  deriving (Show)


scatter :: Material -> Ray -> HitRecord -> Int -> (Vec3, Ray)
scatter material ray hit rndSeed=
  case material.materialType of
    Lambertian -> lambertianScatter material ray hit rndSeed
    Metal -> metalScatter material ray hit rndSeed
    _ -> error "not implemented"


lambertianScatter :: Material -> Ray -> HitRecord -> Int -> (Vec3, Ray)
lambertianScatter material ray hit rndSeed =
  let normal = hit.normal
      d = ray.direction
      seed = rndSeed
      newDirection = (normal + (randomDirection normal seed))
      fixedDirection = if lengthSqr newDirection < 1e-3 then normal else newDirection
      resultRay = Ray hit.point newDirection
  in (material.albedo, resultRay)

metalScatter :: Material -> Ray -> HitRecord -> Int -> (Vec3, Ray)
metalScatter material ray hit rndSeed =
  let normal = hit.normal
      d = ray.direction
      seed = rndSeed
      newDirection = reflect d normal
      fixedDirection = if lengthSqr newDirection < 1e-3 then normal else newDirection
      resultRay = Ray hit.point newDirection
  in (material.albedo, resultRay)


randomDirection :: Vec3 -> Int -> Vec3
randomDirection normal seed =
    let randomUnitVector =
            head  $ dropWhile (\v -> (lengthSqr v) > 1.0)
                                    [ vec3 x y z | (x, y, z) <- zip3 (randomRs (-1.0, 1.0) (mkStdGen seed))
                                                                        (randomRs (-1.0, 1.0) (mkStdGen (seed+10021) ))
                                                                        (randomRs (-1.0, 1.0) (mkStdGen (seed-233) ))]
    in
        if dot randomUnitVector normal > 0
        then randomUnitVector
        else -randomUnitVector
