module TypesExercises where

data Nat = Zero | Succ Nat deriving Show
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
data Expr = Val Int | Add Expr Expr

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n | n < 0 = Zero
          | otherwise = Succ(int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = Succ(add n m)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ n) m = add m (mult n m)

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1

leaves :: Tree a -> Int
leaves (Leaf x) = 1
leaves (Node l r) = leaves l + leaves r

balance :: [a] -> Tree a
balance (x:[]) = Leaf x
balance xs = Node (balance fstHalf) (balance sndHalf)
           where aSplit = split xs
                 fstHalf = fst aSplit
                 sndHalf = snd aSplit

split :: [a] -> ([a],[a])
split [] = ([],[])
split xs = (take half xs , drop half xs)
         where half = ((length xs) `div` 2 + (length xs) `mod` 2)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val i) = f(i)
folde f g (Add e e') = g (folde f g e) (folde f g e')

eval :: Expr -> Int
eval = folde (id) (+)

size :: Expr -> Int
size (Val _) = 1
size (Add e e') = size e + size e'

data Option a = None | Some a

instance Eq a => Eq (Option a) where
Some x == Some y = x == y
None == None = True
_ == _ = False























