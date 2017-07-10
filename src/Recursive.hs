module Recursive(
                fac,
                product2,
                length2,
                reverse2,
                (+++),
                insert',
                insertSort,
                zip',
                drop',
                sumdown,
                (^^^),
                euclid,
                and',
                concat',
                replicate',
                (!!!),
                elem',
                merge,
                msort,
                sum',
                take',
                last'
                ) where

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)
      | otherwise = 0

product2 :: Num x => [x] -> x
product2 [] = 1
product2 (x:xs) = x * product2 xs

length2 :: Num x => [x] -> Int
length2 [] = 0
length2 (_:xs) = 1 + length2 xs

reverse2 :: [x] -> [x]
reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]

(+++) :: [x] -> [x] -> [x]
[] +++ ys = ys
(x:xs) +++ ys = x:(xs +++ ys)

insert' :: Ord x => x -> [x] -> [x]
insert' x [] = [x]
insert' x (x':xs) | x <= x' = x : x' : xs
                 | otherwise = x' : insert' x xs

insertSort :: Ord x => [x] -> [x]
insertSort [] = []
insertSort (x:xs) = insert' x (insertSort xs)

zip' :: [x] -> [y] -> [(x,y)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [x] -> [x]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs

-- exercises

sumdown :: Int -> Int
sumdown n | n >= 0 = n + sumdown (n-1)
          | otherwise = 0

(^^^) :: Int -> Int -> Int
0 ^^^ _ = 0
_ ^^^ 0 = 1
a ^^^ b = a * (a ^^^ (b-1))

euclid :: Int -> Int -> Int
euclid n m | n == m = n
           | n > m = euclid m (n - m)
           | otherwise = euclid n (m - n)

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == True = and' xs
            | otherwise = False

concat' :: [[x]] -> [x]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate'(n-1) a

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(x:xs) !!! n | length xs + 1 > n = xs !!! (n-1)
             | otherwise = x

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (x':xs) | x == x' = True
               | otherwise = elem' x xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x > y = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge (msort (fst h)) (msort (snd h))
          where h = halve xs

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve (x:[]) = ([x], [])
halve xs = (take (d) xs, drop (d + length xs `mod` 2) xs)
          where d = length xs `div` 2

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 xs = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs