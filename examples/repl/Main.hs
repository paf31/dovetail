{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dovetail
import Dovetail.Core (core)
import System.Exit (die)

main :: IO ()
main = do
  either (die . renderInterpretError defaultTerminalRenderValueOptions) pure =<< 
    runInterpret () do
      core
      repl (Just (ModuleName "Prelude"))
      