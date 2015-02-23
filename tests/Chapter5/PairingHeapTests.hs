module Chapter5.PairingHeapTests (pairingHeapTestGroup) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck

import Chapter5.PairingHeap

pairingHeapTestGroup :: Test.Framework.Test
pairingHeapTestGroup = testGroup "Chapter 5 - Pairing Heaps"
    [testGroup "Basic Pairing Heap tests"
        [testProperty "undefined" prop_undefined
        ]
    ]

prop_undefined :: Property
prop_undefined = undefined
