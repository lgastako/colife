{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

-- import Colife ( main )

import Penner             ( run )
import System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= \case
  []   -> run defaultIters
  [n]  -> run (read n)
  args -> putStrLn $ "Couldn't understand args: " ++ show args
  where
    defaultIters = 10
