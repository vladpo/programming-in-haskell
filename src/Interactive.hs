module Interactive where

import System.IO

strLen :: IO Int
strLen = do putStr "Enter a text:"
            s <- getLine
            return (length s)

hangman :: IO ()
hangman = do putStr "Enter a secret word:"
             w <- egetLine
             putStrLn "Try to guess it:"
             play w

egetLine :: IO String
egetLine = do c <- getCh
              if c == '\n' then
                 do putChar c
                    return []
              else
                 do putChar '-'
                    cs <- egetLine
                    return (c:cs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play s = do putStr "? "
            guess <- getLine
            if s == guess then
               do putStr "That's right"
                  return ()
            else
               do putStr "Try again"
                  play s