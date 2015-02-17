module Main where

import Test.Framework (defaultMain)

import Chapter2Tests
import Chapter3.LeftistHeapTests

main :: IO ()
main = defaultMain
    [chapter2TestGroup
    ,leftistHeapTestGroup
    ]
