module HigherOrderFunctions(
                            twice,
                            map',
                            sumSqrEvens,
                            foldRight,
                            reverse',
                            sum',
                            foldLeft,
                            bin2int,
                            int2bin,
                            make8,
                            encode,
                            chop8,
                            transmit
                            )
                            where
import Data.Char

twice :: (a -> a) -> a -> a
twice f a = f (f a)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

sumSqrEvens :: [Int] -> Int
sumSqrEvens = sum . map (^2) . filter even

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f v [] = v
foldRight f v (x:xs) = f x (foldRight f v xs)

{-
    [1,2,3,4,5]
    (((([] ++ [5]) ++ [4])) ++ [3]) ++ [2]) ++ [1]
    (:') :: a -> [a] -> [a]
    x :' xs = xs ++ [x]
    -----------------------------------------------
    1 :' (2 :' (3 :' (4 :' (5 :' []))))
-}
reverse' :: [a] -> [a]
reverse' = foldRight snc []

-- reverse of cons
snc :: a -> [a] -> [a]
snc x xs = xs ++ [x]

-- sum (x:xs) = x + sum xs          =>   1 + (2 + (3 + (4 + 0))) fold right, starts from right
-- sum (x:xs) = sum xs + x          =>   (((0 + 4) + 3) + 2) + 1 fold right, starts from right
-- sum v (x:xs) = sum (v + x) xs    =>   (((0 + x0) + x1) + x2) +x3 => (((0 + 1) + 2) + 3) + 4 fold left, starts from left
sum' :: Num a => [a] -> a
sum' = loop 0
        where
            loop v [] = v
            loop v (x:xs) = loop (v + x) xs

foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft f v [] = v
foldLeft f v (x:xs) = foldLeft f (f v x) xs -- (((0 + 1) + 2) + 3) + 4

-- Binary string transmitter

type Bit = Int

-- abcd => (a * 2^0) + (b * 2^1) + (c * 2^2) + (d * 2^3) = a + 2*b + 4*c + 8*d = a + 2*(b + 2*(c + 2*(d + 2*0)))
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = [0]
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bs = take 8 (bs ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bs = take 8 bs : chop8(drop 8 bs)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id