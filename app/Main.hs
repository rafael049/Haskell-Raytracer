{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Data.Maybe (catMaybes)

import qualified Core

screenWidth = 1280 :: Int
screenHeight = 720 :: Int

main :: IO ()
main = do
  let core = Core.core screenWidth screenHeight
  Core.render core 

