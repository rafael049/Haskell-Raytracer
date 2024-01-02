{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Data.Maybe (catMaybes)

import qualified Core

screenWidth = 640 :: Int
screenHeight = 360 :: Int

main :: IO ()
main = do
  let core = Core.core screenWidth screenHeight
  Core.render core 

