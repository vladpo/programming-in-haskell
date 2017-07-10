module Lib
    ( luhnDouble, luhn, firsts, factors, prime, primes, find, pairs, sorted, positions, lowers, count,
    mult,
      someFunc,
      double,
      factorial,
      average,
      average2,
      some,
      myLast,
      myLast2,
      myInit,
      myInit2,
      add2,
      listStartsWithA,
      halve,
      third,
      third2,
      third3,
      apply,
      nonEmpty,
      safeTail,
      safeTail2,
      safeTail3,
      (&&&)
    ) where

someFunc :: IO ()
-- someFunc = mapM_ print (seqn [getChar, getChar, getChar])
-- someFunc = print (myProduct [2,3,4])
someFunc = print (qsort [6,2,3,8,5,1])

qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                smaller = [a | a <- xs, a <= x]
                larger = [b | b <- xs, b > x]

seqn [] = []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

double x = x * 2
quadruple x = double (double x)
add2 x y = x + y

myProduct [] = 0
myProduct (x:xs) = x * product xs

factorial n = product [1..n]
average xs = sum xs `div` length xs
average2 a b = sum [a,b] `div` 2

some = a `div` (length xs)
       where
       a = 10
       xs = [1,2,3,4,5]
myLast ns = head (reverse ns)
myLast2 ns = head (drop (length ns - 1) ns)
myInit ns = take (length ns - 1) ns
myInit2 ns = reverse (drop 1 (reverse ns))

listStartsWithA :: [Char] -> Bool
listStartsWithA ('a':_) = True
listStartsWithA _ = False

halve :: [a] -> ([a], [a])
halve xs | length xs `mod` 2 == 0 = (take hl xs, drop hl xs)
         | otherwise = ([],[])
         where hl = length xs `div` 2

third :: [a] -> a
third xs | moreThan3 xs = head (apply (tail) xs 2)

third2 :: [a] -> a
third2 xs | moreThan3 xs = xs !! 2

third3 :: [a] -> a
third3 (_:_:a:_) = a

moreThan3 :: [a] -> Bool
moreThan3 xs = length xs > 2

apply :: (a -> a) -> a -> Int -> a
apply f x n
    | n > 0 = apply (f) (f(x)) (n - 1)
    | otherwise = x

nonEmpty :: [a] -> Bool
nonEmpty (a:_) = True
nonEmpty _ = False

safeTail :: [a] -> [a]
safeTail as | nonEmpty as = tail as
            | otherwise = as

safeTail2 :: [a] -> [a]
safeTail2 as = if nonEmpty as then tail as else []

safeTail3 :: [a] -> [a]
safeTail3 (_:as) = as
safeTail3 [] = []

(&&&) :: Bool -> Bool -> Bool
a &&& b = if a then b else a

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y *z))

luhnDouble :: Int -> Int
luhnDouble a | a + a > 9 = a + a - 9
             | otherwise = a + a

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (a + (luhnDouble b) + c + (luhnDouble d)) `mod` 10 == 0

firsts :: [(a,b)] -> [a]
firsts xs = [x | (x,_) <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq k => k -> [(k,v)] -> [(k,v)]
find k xs = [(k, v) | (k1, v) <- xs, k == k1]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Ord x => x -> [x] -> [Int]
positions x xs = [i | (y, i) <- zip xs [0..], x == y]

lowers :: String -> Int
lowers s = length [c | c <- s, c >= 'a' && c <= 'z']

count :: Char -> String -> Int
count c s = length [cc | cc <- s, cc == c]