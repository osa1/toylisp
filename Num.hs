module Num where

--import Types
import Data.Char (toLower)

numList :: [(Char, Int)]
numList =  [ ('0', 0)
           , ('1', 1)
           , ('2', 2)
           , ('3', 3)
           , ('4', 4)
           , ('5', 5)
           , ('6', 6)
           , ('7', 7)
           , ('8', 8)
           , ('9', 9)
           , ('a', 10)
           , ('b', 11)
           , ('c', 12)
           , ('d', 13)
           , ('e', 14)
           , ('f', 15)
           ]


nlookup :: Char -> Int
nlookup n = case lookup (toLower n) numList of
    Nothing -> error $ "can't convert " ++ show n ++ " to Int."
    Just c  -> c

toInt :: String -> Int -> Int
toInt num base = foldr r 0 (zip num $ reverse [0..(length num)])
    where r :: (Char, Int) -> Int -> Int
          r (c, p) e = e + ((nlookup c)*(base^(p-1)))


toFloat :: String -> String -> Int -> Float
toFloat i frac base = (fromIntegral (toInt i base) :: Float) + (foldl r 0 (zip frac [0..]))
    where r :: Float -> (Char, Int) -> Float
          r t (c, p) = t + (1/(fromIntegral (nlookup c) :: Float)**(fromIntegral p :: Float))