module Lib
    (filterWithApply
    ,all
    ) where

filterWithApply :: Eq a => (a -> b) -> (a -> Bool) -> [a] -> [b]
filterWithApply f p = map f . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr ((&&).p) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr ((||) . p) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if p x then x:takeWhile' p xs else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else x:xs

filterWithApply' :: Eq a => (a -> b) -> (a -> Bool) -> [a] -> [b]
filterWithApply' f p = foldr (\x xs -> if p x then f x : xs else xs) []


