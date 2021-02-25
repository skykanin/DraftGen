module Main where

import CLI (Args, Unwrapped, unwrapRecord)

main :: IO ()
main = do
    x <- unwrapRecord "Test Program"
    print (x :: Args Unwrapped)
