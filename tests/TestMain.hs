module Main where

import Test.Framework (defaultMain)

import Chapter2Tests
import Chapter3Tests

main :: IO ()
main = defaultMain
    [chapter2TestGroup
    ,chapter3TestGroup
    ]
