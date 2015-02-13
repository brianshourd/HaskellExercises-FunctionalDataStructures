module Main where

import Test.Framework (defaultMain)

import Chapter2Tests

main :: IO ()
main = defaultMain
    [chapter2TestGroup
    ]
