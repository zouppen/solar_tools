module Main where

import Common.DbHelpers (initSharedDb)
import Common.ConfigHelpers (readConfigFromArg)

import Integrator.Integrator (runIntegrator)

main :: IO ()
main = do
  conf <- readConfigFromArg
  sharedDb <- initSharedDb
  runIntegrator sharedDb conf
