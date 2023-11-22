{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Core where

import Data.Maybe (catMaybes)
import System.Random
import Debug.Trace

import Image
import Vector
import Camera
import Ray
import qualified Hittable

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
  }

core :: Int -> Int -> Core
core width height =
  let
    camera = Camera (vec3 0 (-0.1) (-0.5)) (vec3 0 0 1) 1.0
    world = [ Hittable.Sphere (vec3 0 0 (-1)) 0.3
            ,  Hittable.Sphere (vec3 0.0 (-100.3) (-1)) 100
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
     }


render :: Core -> IO ()
render core = do

  let finalScreen = map (drawColor core) core.screenPixels

  writeImage finalScreen (core.screenWidth, core.screenHeight) "teste.ppm"

  where

    drawColor :: Core -> (Float, Float) -> Vec3
    drawColor core (i, j) =
      let
        sampleRays = createSamplesRays core (i, j) core.samplesPerPixel
        samplesColors = map (\r -> rayColor r core.world) sampleRays
        averageColor = fmap (/(fromIntegral core.samplesPerPixel)) $ sum samplesColors
      in
        averageColor

    createSamplesRays :: Core -> (Float, Float) -> Int -> [Ray]
    createSamplesRays core (i, j) samples =
      let pixelCenter = core.pixel00 + (j *^ core.pixelDeltaU) + (i *^ core.pixelDeltaV)
          pixelsSamples = [ (vec3 (pixelCenter.x + du * core.pixelDeltaU.x)
                            (pixelCenter.y + dv * core.pixelDeltaV.y)
                             pixelCenter.z)
                          | (du, dv) <- zip (take core.samplesPerPixel $ randomRs (-0.5, 0.5) (mkStdGen 12))
                                            (take core.samplesPerPixel $ randomRs (-0.5, 0.5) (mkStdGen 13))
                          ]
          raysDirection = map (\p -> p - core.camera.position) pixelsSamples 
      in
          map (Ray core.camera.position) raysDirection

  
rayColor :: Ray -> [Hittable.Sphere] -> Vec3
rayColor ray hittableList =
  let tMin = 0.0
      tMax = 100.0
      maybeHit =  (closestHit ray . catMaybes) $ map (\x -> Hittable.hit x ray tMin tMax) hittableList 
  in
    case maybeHit of
      Nothing -> 
        let unit = norm ray.direction
            a = (1.0 + unit.y) / 2.0
        in (1.0 - a) *^ (vec3 1.0 1.0 1.0) + a *^ (vec3 0.5 0.7 1.0)

      Just hit ->
        let normal = hit.normal
        in fmap (\x -> x/2.0 + 0.5) $ vec3 normal.x normal.y normal.z

  where
    closestHit :: Ray -> [Hittable.HitRecord] -> Maybe Hittable.HitRecord
    closestHit _ [] = Nothing
    closestHit ray list = return $ foldr1 (\hit acc ->
                                   if hit.t < acc.t
                                   then hit
                                   else acc
                                ) list
