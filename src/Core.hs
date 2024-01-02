{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BangPatterns #-}

module Core where

import Data.Maybe (catMaybes)
import System.Random
import Debug.Trace
import Control.Parallel.Strategies (parList, rdeepseq, using)

import Image
import Vector
import Camera
import Ray
import qualified Hittable
import HitRecord
import qualified Material

data Core = Core
  { camera :: Camera
  , world :: [Hittable.Sphere]
  , screenWidth :: Int
  , screenHeight :: Int
  , aspectRatio :: Float
  , viewportWidth :: Float
  , viewportHeight :: Float
  , viewportU :: Vec3
  , viewportV :: Vec3
  , pixelDeltaU :: Vec3
  , pixelDeltaV :: Vec3
  , viewportUpperLeft :: Vec3
  , pixel00 :: Vec3
  , screenPixels :: [(Float, Float)]
  , samplesPerPixel :: Int
  , bounces :: Int
  }

sphere1 = Hittable.Sphere (vec3 0 0 (-1)) 0.3 (Material.Material (vec3 0.2 0.8 0.8) Material.Metal)
sphere2 = Hittable.Sphere (vec3 0.0 (-100.3) (-1)) 100 (Material.Material (vec3 0.7 0.7 0.2) Material.Lambertian)
sphere3 = Hittable.Sphere (vec3 0.1 (-0.2) (-0.7)) 0.05 (Material.Material (vec3 0.2 0.8 0.8) Material.Lambertian)

core :: Int -> Int -> Core
core width height =
  let
    camera = Camera (vec3 0 (-0.2) (-0.3)) (vec3 0 0 1) 1.0
    world = [ sphere1
            , sphere2
            , sphere3
            ]
    aspectRatio = (fromIntegral width) / (fromIntegral height)
    viewportHeight = 2.0
    viewportWidth = viewportHeight * aspectRatio
    viewportU = vec3 viewportWidth 0 0
    viewportV = vec3 0 (-viewportHeight) 0
    pixelDeltaU = viewportU ^/ (fromIntegral width)
    pixelDeltaV = viewportV ^/ (fromIntegral height)
    viewportUpperLeft = camera.position - (vec3 0 0 camera.focalLength) - viewportU^/2.0 - viewportV^/2.0
    pixel00 = viewportUpperLeft + (0.5 *^ (pixelDeltaU + pixelDeltaV))
    screenPixels = [ (fromIntegral i, fromIntegral j) | i <- [0..(height-1)], j <- [0..(width-1)] ]
    samplesPerPixel = 32
    bounces = 10
  in Core
     { camera = camera
     , world = world
     , screenWidth = width
     , screenHeight = height
     , aspectRatio = aspectRatio
     , viewportWidth = viewportWidth
     , viewportHeight = viewportHeight
     , viewportU = viewportU
     , viewportV = viewportV
     , pixelDeltaU = pixelDeltaU
     , pixelDeltaV = pixelDeltaV
     , viewportUpperLeft = viewportUpperLeft
     , pixel00 = pixel00
     , screenPixels = screenPixels
     , samplesPerPixel = samplesPerPixel
     , bounces = bounces
     }


render :: Core -> IO ()
render core = do
  finalScreen <- mapM (drawPixel core) core.screenPixels
  putStrLn "writing image"
  --writeImage finalScreen (core.screenWidth, core.screenHeight) "teste.ppm"

  where

    gammaCorrection :: Vec3 -> Vec3
    gammaCorrection = fmap sqrt

    drawPixel :: Core -> (Float, Float) -> IO Vec3
    drawPixel core (i, j) = do
        rndSeed <- randomIO :: IO Int
        let sampleRays = createSamplesRays core (i, j) rndSeed
        samplesColors <- mapM (\r -> rayColor r core.world core.bounces) sampleRays
        let averageColor = fmap (/(fromIntegral core.samplesPerPixel)) $! sum samplesColors
        return $ gammaCorrection averageColor

    createSamplesRays :: Core -> (Float, Float) -> Int -> [Ray]
    createSamplesRays core (i, j) rndSeed =
      let pixelCenter = core.pixel00 + (j *^ core.pixelDeltaU) + (i *^ core.pixelDeltaV)
          randomValues = take (2 * core.samplesPerPixel) $ randomRs (-0.5, 0.5) (mkStdGen rndSeed)
          f (du:dv:[]) = (vec3 (pixelCenter.x + du * core.pixelDeltaU.x)
                               (pixelCenter.y + dv * core.pixelDeltaV.y)
                               pixelCenter.z) : []
          f (du:dv:t) = let v = (vec3 (pixelCenter.x + du * core.pixelDeltaU.x)
                                      (pixelCenter.y + dv * core.pixelDeltaV.y)
                                      pixelCenter.z)
                        in v : (f t)
          pixelsSamples = f randomValues
          raysDirection = map (\p -> p - core.camera.position) pixelsSamples 
      in
          map (Ray core.camera.position) raysDirection

  
rayColor :: Ray -> [Hittable.Sphere] -> Int -> IO Vec3
rayColor _ _ 0 = return $ vec3 0 0 0
rayColor ray hittableList depth = do
  rndSeed <- randomIO :: IO Int
  let tMin = 0.001
      tMax = 100.0
      maybeHit = (closestHit ray . catMaybes) $ map (\x -> Hittable.hit x ray tMin tMax) hittableList 
  case maybeHit of
    Nothing -> 
      return $! backGroundColor ray
    Just (hit, material) -> do
      let (attenuation, newRay) = Material.scatter material ray hit rndSeed
      result <- rayColor newRay hittableList (depth-1)
      return $! attenuation * result

  where

    closestHit :: Ray -> [(HitRecord, Material.Material)] -> Maybe (HitRecord, Material.Material)
    closestHit _ [] = Nothing
    closestHit ray list = return $! foldl1 (\(hit, mat) (acc, matAcc) ->
                                   if hit.t < acc.t
                                   then (hit, mat)
                                   else (acc, matAcc)
                                ) list

    backGroundColor :: Ray -> Vec3
    backGroundColor !ray =
        let unit = norm ray.direction
            a = (1.0 + unit.y) / 2.0
        in (1.0 - a) *^ (vec3 1.0 1.0 1.0) + a *^ (vec3 0.7 0.7 1.0)
