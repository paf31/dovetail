{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Dovetail
import Dovetail.Core qualified as Core
import System.Exit (die)

main :: IO ()
main = do
  either (die . renderInterpretError defaultTerminalRenderValueOptions) pure =<< 
    runInterpret () do
      Core.buildModules Core.minimal
      repl (Just (ModuleName "Prelude"))
      