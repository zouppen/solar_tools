{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}
module Main where

import Common.DbHelpers (initSharedDb)
import Common.ConfigHelpers (getConfigFile)

import Binner.Binner (runBinner)

main :: IO ()
main = do
  confFile <- getConfigFile
  sharedDb <- initSharedDb
  runBinner confFile sharedDb
