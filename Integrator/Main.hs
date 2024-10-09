module Main where

import Common.ConfigHelpers (readConfigAndDatabaseFromArg)

import Integrator.Integrator (runIntegrator)

main :: IO ()
main = readConfigAndDatabaseFromArg >>= uncurry runIntegrator
