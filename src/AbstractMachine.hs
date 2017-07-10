module AbstractMachine where

data Expr = Var Int | Add Expr Expr | Mult Expr Expr
data Op = EVAL Int Expr | ADD Int | MULT Int
type Cont = [Op]

value :: Expr -> Int
value e = eval e []

eval :: Expr -> Cont -> Int
eval (Var n) c = exec c n
eval (Add x y) c = eval x (EVAL 0 y : c)
eval (Mult x y) c = eval x (EVAL 1 y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL 0 y : c) n = eval y (ADD n : c)
exec (EVAL 1 y : c) n = eval y (MULT n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)