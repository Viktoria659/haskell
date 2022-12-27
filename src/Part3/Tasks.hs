module Part3.Tasks where

import Util (notImplementedYet)
import Data.List

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n =  f(n) : finc f (n+1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f(x))

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
toDigits :: Int -> [Int]
toDigits x | x < 10 = [x]
    | otherwise = toDigits (div x 10) ++ [mod x 10]

toList:: [Int] -> [Int]
toList [] = []
toList (x:xs) = toDigits x ++ (toList xs)

histogram :: Eq a => [a] -> [(Int, a)]
histogram xs = map (\x -> (length $ filter (x==) xs, x)) $ nub xs

mostFreq :: [Int] -> Int
mostFreq a = snd $ maximum $ histogram $ toList a

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq a = nubMy a []

nubMy :: (Eq a) => [a] -> [a] -> [a]
nubMy [] a =  a
nubMy (x:xs) a = if elem x a then (nubMy xs a) else (nubMy xs (x : a))

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = map (\x -> (x, filter (\y -> (f y) == x) l)) $ nub res where
  res = map f l