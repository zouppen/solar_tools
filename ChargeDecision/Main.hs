module Main where

import Common.ConfigHelpers (readConfigAndDatabaseFromArg)

import ChargeDecision.ChargeDecision (runChargeDecision)

main :: IO ()
main = readConfigAndDatabaseFromArg >>= uncurry runChargeDecision
