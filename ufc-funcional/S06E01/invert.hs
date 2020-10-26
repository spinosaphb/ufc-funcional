{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

rot xs n = (drop n xs)++(take n xs)

main :: IO()
main = do
    ndTemp <- getLine
    let nd = words ndTemp

    let n = read (nd !! 0) :: Int

    let d = read (nd !! 1) :: Int

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . words $ aTemp
        a2 = rot a n
    putStrLn $ unwords [show x|x <- a2]