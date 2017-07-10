module CaesarCipher(replicate2, pyths, perfects,
        char2int,
         int2char,
         shift,
         encode,
         percent,
         freqs,
         rotate,
         powerSum,
         grid,
         square) where

import Data.Char

crack :: String -> String
crack cs = encode (-e) cs
           where e = head (positions (minimum chiSquares) chiSquares)
                 chiSquares = [chiSquare (rotate i (fqs)) expected | i <- [0..51]]
                 fqs = freqs cs

encode :: Int -> String -> String
encode n s = [shift n c | c <- s]

shift :: Int -> Char -> Char
shift n c | char2int c <= 25 = int2char ((char2int c + n) `mod` 26)
          | char2int c >= 26 && (char2int c <= 51) = shiftUpper n c
          | otherwise = c

shiftUpper :: Int -> Char -> Char
shiftUpper n c | m <= 25 = int2char(26 + m)
               | m >=26 = int2char(m)
                where m = (char2int c + n) `mod` 52

char2int :: Char -> Int
char2int c | isBetween c 'a' 'z' = ord c - ord 'a'
           | isBetween c 'A' 'Z' = ord c - ord 'A' + 26
           | otherwise = 51 + ord c

int2char :: Int -> Char
int2char i | isBetween' i 'a' 'z' = chr (ord 'a' + i)
           | isBetween' i 'A' 'Z' = chr (ord 'A' + i - 26)
           | otherwise = chr i

isBetween :: Char -> Char -> Char -> Bool
isBetween c c' c'' = ord c' <= (ord c) && (ord c) <= (ord c'')

isBetween' :: Int -> Char -> Char -> Bool
isBetween' i c' c'' | i <= 25 = isBetween (chr (i + (ord 'a'))) c' c''
                    | i >= 26 && i <= 51 = isBetween (chr (i - 26 + (ord 'A'))) c' c''
                    | otherwise = False

positions :: Ord x => x -> [x] -> [Int]
positions x xs = find x (zip xs [0..(length xs)])

chiSquare :: [Float] -> [Float] -> Float
chiSquare os es = sum [(o - e)^2 / e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ (take n xs)

freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a'..'z'] ++ ['A'..'Z']]
           where n = countLetters cs

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Char -> String -> Int
count c s = length [cc | cc <- s, cc == c]

countLetters :: String -> Int
countLetters s = countChars s 'A' 'Z' + (lowers s)

countChars :: String -> Char -> Char -> Int
countChars s c' c'' = length [c | c <- s, c >= c' && c <= c'']

lowers :: String -> Int
lowers s = countChars s 'a' 'z'

expected :: [Float]
expected = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1, 11.6, 4.7, 3.5, 2.7, 2.0, 3.8, 1.9, 7.2, 6.3, 0.6, 0.6, 2.7, 4.4, 2.3, 6.2, 2.5, 0.2, 1.6, 7.7, 16.6, 1.5, 0.6, 6.7, 0.017, 1.62, 0.034]

-- exercises
powerSum :: Int -> Int
powerSum n = sum [x^2 | x <- [1..n]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

replicate2 :: Int -> a -> [a]
replicate2 n a = [a | _ <- [0..(n-1)]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | (x,y) <- square n, z <- [(max x y + 1)..n],  x /= 0 && y /= 0 && x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [p | p <- [4..n], sum (init (factors p)) == last (factors p)]

find :: Eq k => k -> [(k,v)] -> [v]
find k xs = [v | (k1, v) <- xs, k == k1]

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [ x*y | (x, y) <- zip xs ys]