module Main where

import Common.ConfigHelpers (readConfigAndDatabaseFromArg)

import Binner.Binner (runBinner)

main :: IO ()
main = readConfigAndDatabaseFromArg >>= uncurry runBinner
