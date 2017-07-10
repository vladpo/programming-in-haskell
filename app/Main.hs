module Main where

import Data.List

main :: IO ()
main = print (ordSolutions [1,3,7,10,25,50] 50)

data Op = Add | Sub | Mul | Div | Pow deriving Eq
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                   where
                      brak (Val n) = show n
                      brak e = "(" ++ show e ++ ")"
instance Eq Expr where
  e == e' = values e == values e' && operations e == operations e'
instance Ord Expr where
  compare e e' = compare (values e) (values e')

operations :: Expr -> [Op]
operations (Val _) = []
operations (App o p q) = operations p ++ [o] ++ (operations q)

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Sub n m = n > m
valid Div n m = m /= 1 && m /= 0 && n `mod` m == 0
valid Pow n m = n /= 1 && m /= 1 && m > 0

apply :: Op -> Int -> Int -> Int
apply Add n m = n + m
apply Sub n m = n - m
apply Mul n m = n * m
apply Div n m = n `div` m
apply Pow n m = n^m

values :: Expr -> [Int]
values (Val n) = [n]
values (App o l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yys ++ map (x:) yys
            where yys = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

choices :: [a] -> [[a]]
--choices xs = [ys | xxs <- subs xs, ys <- perms xxs]
choices = concatMap perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e xs r = elem (values e) (choices xs) && eval e == [r]

split :: [a] -> [([a], [a])]
split [] = [([],[])]
split [_] = [([],[])]
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l        <- exprs ls,
                r        <- exprs rs,
                e        <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Pow]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [ res | (ls, rs) <- split ns,
                      l <- results ls,
                      r <- results rs,
                      res <- combine' l r]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] xs = True
isChoice ys [] = False
isChoice (y:ys) xs | length xs > length xs' =  isChoice ys xs'
                   | otherwise = False
                   where xs' = rmFst y xs

rmFst :: Eq a => a -> [a] -> [a]
rmFst x xs = [y | y <- xs, y/=x]

possible :: [Int] -> Int
possible xs = length [e | ys <- choices xs, e <- exprs ys, eval e == [765]]

nearSolutions :: [Int] -> Int -> ([Expr], Int)
nearSolutions ns n | length (sol) > 0 = (sol, n)
                   | otherwise = nearSolutions ns (n+1)
                   where sol = solutions' ns n

ordSolutions :: [Int] -> Int -> ([Expr], Int)
ordSolutions ns n = (sort (fst (sol)), snd sol)
                  where sol = nearSolutions ns n




























