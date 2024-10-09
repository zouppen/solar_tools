module Main where

import Common.DbHelpers (initSharedDb)
import Common.ConfigHelpers (readConfigFromArg)

import ChargeDecision.ChargeDecision (runChargeDecision)

main :: IO ()
main = do
  conf <- readConfigFromArg
  sharedDb <- initSharedDb
  runChargeDecision sharedDb conf
