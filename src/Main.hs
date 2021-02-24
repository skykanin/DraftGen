module Main where

import CLI (Args, getRecord)

main :: IO ()
main = do
    x <- getRecord "Test Program"
    print (x :: Args)
