module Image (
  writeImage
  )where

import qualified Data.ByteString.Char8 as BS

import Vector
  

ppmHeader :: (Int, Int) -> String
ppmHeader (width, height) = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"

writeImage :: [Vec3] -> (Int, Int) -> String -> IO ()
writeImage pixels (width, height) path =
  let header = ppmHeader (width, height)
      body = unlines $ map mapColorToPPMString pixels
  in BS.writeFile path $ BS.pack $ header ++ body
  
mapColorToPPMString :: Vec3 -> String
mapColorToPPMString (Vector3 x y z) = unwords $ map (show . floor . (*255)) [x, y, z]
