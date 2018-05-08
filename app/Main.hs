module Main where

import           NoPoly
import           Poly1
import           Poly2
import           Poly3
import           Poly4
import           Poly5

main :: IO ()
main = do
    putStrLn "\nNoPoly.someFunc:"
    NoPoly.someFunc
    putStrLn "\nPoly1.someFunc:"
    Poly1.someFunc
    putStrLn "\nPoly2.someFunc:"
    Poly2.someFunc
    putStrLn "\nPoly3.someFunc:"
    Poly3.someFunc
    putStrLn "\nPoly4.someFunc:"
    Poly4.someFunc
    putStrLn "\nPoly5.someFunc:"
    Poly5.someFunc
