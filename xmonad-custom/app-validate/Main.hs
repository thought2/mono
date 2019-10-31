module Main where

import qualified XMonadCustom.EnvConfig

main :: IO ()
main = do
  _ <- XMonadCustom.EnvConfig.getUnsafe
  pure ()
