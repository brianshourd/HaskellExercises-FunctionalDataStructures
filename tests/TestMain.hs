module Main where

import Test.Framework (defaultMain)

import Chapter2Tests
import Chapter3.LeftistHeapTests
import Chapter3.BinomialHeapTests
import Chapter3.RedBlackTreeTests

main :: IO ()
main = defaultMain
    [chapter2TestGroup
    ,leftistHeapTestGroup
    ,binomialHeapTestGroup
    ,redBlackTreeTestGroup
    ]
