module Chapter3Tests (chapter3TestGroup) where

import Test.Framework (testGroup, Test)
--import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

--import Test.HUnit
--import Test.QuickCheck (Gen, choose, Property, forAll)

import Chapter3

chapter3TestGroup :: Test.Framework.Test
chapter3TestGroup = testGroup "Chapter 3"
    [testGroup "Exercise 3.2: Merge-free insert"
        [testProperty "insert' works same as insert" prop_insertSame
        ]
    ]

prop_insertSame :: [Char] -> Bool
prop_insertSame xs = buildWith insert xs == buildWith insert' xs where
    buildWith ins = foldl (flip ins) E
