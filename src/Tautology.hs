module Tautology where

type Assoc k v = [(k, v)]
first :: Eq k => k -> Assoc k v -> v
first k a = head [v | (k', v) <- a, k == k']

type Subst = Assoc Char Bool
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop

eval :: Prop -> Subst -> Bool
eval (Const b) s = b
eval (Var c) s = first c s
eval (Not p) s = not (eval p s)
eval (And p p') s = eval p s && eval p' s
eval (Imply p p') s = eval p s <= eval p' s
eval (Or p p') s = eval p s || eval p' s

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ (map (True:) bss)
        where bss = bools (n - 1)

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p p') = vars p ++ (vars p')
vars (Imply p p') = vars p ++ (vars p')
vars (Or p p') = vars p ++ (vars p')

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : (rmdups (filter (/=x) xs))

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length (vs)))
         where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval p s | s <- substs p]