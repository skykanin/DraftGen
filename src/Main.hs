module Main where

import CLI (Args, Unwrapped, unwrapRecord)

main :: IO ()
main = do
    x <- unwrapRecord ""
    print (x :: Args Unwrapped)
