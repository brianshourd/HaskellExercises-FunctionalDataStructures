module Chapter5.PairingHeap where

data Heap a = E | T a [Heap a]
