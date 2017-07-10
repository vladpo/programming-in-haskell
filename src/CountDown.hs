module CountDown where

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add n m = n + m > 0
valid Sub n m = n - m > 0
valid Mul n m = n * m > 0
valid Div n m = n `mod` m == 0

apply :: Op -> Int -> Int -> Int
apply Add n m = n + m
apply Sub n m = n - m
apply Mul n m = n * m
apply Div n m = n `div` m

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val x) = show x
  show (App o l r) = brak l ++ show o ++ brak r
                   where
                      brak (Val n) = show n
                      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ p q) = values p ++ values q

eval :: Expr -> [Int]
eval (Val n) = [n | n>0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yys ++ map (x:) yys
            where yys = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concatMap perms . subs

exprs :: [Int] -> [Expr]
exprs [] = []
exprs













































