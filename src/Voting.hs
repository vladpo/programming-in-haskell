module Voting(count
        ,winner
        ,votes
        ,indexCount
        ,win
        ,main
        ,map'
        ,dec2int
        ,curry'
       ) where

import Data.List

main :: IO ()
main = do
    putStr (foldl (\s xs -> s ++ unwords xs ++ "\n") "" (win 0 votes))

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmDups :: Eq a => [a] -> [a]
rmDups [] = []
rmDups (x:xs) = x : rmDups (filter (/=x) xs)

winner :: Eq a => [a] -> a
winner xs = snd (foldr (\x (c, winner) -> if (count x xs > c) then (count x xs,x) else (c, winner)) (0, head xs) noDups)
          where
            noDups = rmDups xs

votes :: [[String]]
votes = [ ["Red", "Blue", "Green"]
        , ["Red", "Green"]
        , ["Green", "Red", "Red"]
        , ["Blue", "Green", "Green"]
        , ["Green", "Red", "Green"]
        ]

win :: Eq a => Int -> [[a]] -> [[a]]
win i xss
        | i == maxSize xss - 1 = xss
        | otherwise = win (i + 1) (sortOn (countVotes) xss)
                    where
                        countVotes = \xs -> if (length xs > i) then indexCount i (xs !! i) xss else 0

maxSize :: [[a]] -> Int
maxSize = foldl (\m xs -> if (length xs > m) then length xs else m) 0

indexCount :: Eq a => Int -> a -> [[a]] -> Int
indexCount i x = foldl (\c xs-> if (x == xs !! i) then c + 1 else c) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x ys -> f x : ys) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x xs' -> if f x then x:xs' else xs') [] xs

dec2int :: [Int] -> Int
dec2int xs = fst (foldl (\t x -> (fst t + snd t * x, snd t `div` 10)) (0,10 ^ (length xs - 1)) xs)

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x -> \y -> f (x,y)

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 bs = unfold (\bs' -> 0 == (length bs')) (take 8) (drop 8) bs

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold isEmpty (\ys -> f (head ys)) tail

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = foldr