{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}
module Main where

import Common.DbHelpers (initSharedDb)
import Common.ConfigHelpers (readConfigFromArg)

import Binner.Binner (runBinner)

main :: IO ()
main = do
  conf <- readConfigFromArg
  sharedDb <- initSharedDb
  runBinner sharedDb conf
