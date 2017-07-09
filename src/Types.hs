module Types where

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero Nat = Nat
add (Succ n) m = Succ (add n m)

mult :: Nat -> Nat -> Nat


