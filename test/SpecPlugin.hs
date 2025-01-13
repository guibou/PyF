{-# OPTIONS -fplugin=PyF.Plugin #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import PyF

main = do
  putStrLn [fmt|Hello world! {(1 :: Int) + 1:s} is a nice value.|]
