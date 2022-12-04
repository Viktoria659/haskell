module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin z = f 0 0 z 1

f :: Double -> Double -> Double -> Double -> Double
f ac 50 _ _ = ac
f ac n z c = f (ac + xx n z c) (n + 1) z c

xx :: Double -> Double -> Double -> Double
xx k z c =  ((-1) ^ (truncate k)) * z ^ (2 * (truncate k) + truncate c) / fac (2 * k + c)

fac :: Double -> Double
fac 0 = 1
fac 1 = 1
fac n = n * fac (n - 1)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos z = f 0 0 z 0

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x y | x == y = x
       | x == 0 = y
       | y == 0 = x
       | otherwise = if x > y then fanGCD x y else fanGCD y x

fanGCD :: Integer -> Integer -> Integer
fanGCD x y = if (mod x y) == 0 then y else fanGCD y (mod x y)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y | d <= 0 = False
       | d >= 32 = False
       | m <= 0 = False
       | m >= 13 = False
       | y <= 0 = False
       | otherwise = isDayCorrect d m (((mod y 4 == 0) && (mod y 100 > 0)) || (mod y 400 == 0))

isDayCorrect :: Integer -> Integer -> Bool -> Bool
isDayCorrect d m v = if (m == 1 && d <= 31) then True else
  if (m == 2 && d <= (if v == False then 28 else 29)) then True else
  if (m == 3 && d <= 31) then True else
  if (m == 4 && d <= 30) then True else
  if (m == 5 && d <= 31) then True else
  if (m == 6 && d <= 30) then True else
  if (m == 7 && d <= 31) then True else
  if (m == 8 && d <= 31) then True else
  if (m == 9 && d <= 30) then True else
  if (m == 10 && d <= 31) then True else
  if (m == 11 && d <= 30) then True else
  if (m == 12 && d <= 31) then True else False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x y | (y == 0 && x == 0) = error "args musn't be 0"
       | y == 0 = 1
       | x == 0 = 0
       | y == 1 = x
       | y < 0 =  error "exponentiation number must be >= 0"
       | otherwise = myExpon 1 x y

myExpon ::  Integer -> Integer -> Integer -> Integer
myExpon ac _ 0 = ac
myExpon ac x y = myExpon (ac * x) x (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n | n <= 2 = True
       | otherwise = myIsPrime n 2

myIsPrime :: Integer -> Integer -> Bool
myIsPrime n i = if i * i <= n then (if rem n i == 0 then False else myIsPrime n (i + 1))
else True

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = abs $ 0.5 * (myShapePl (head points) points + myShapeMn (head points) points)

myShapePl :: (Double, Double) -> [Point2D] -> Double
myShapePl h [y] = fst y * snd h
myShapePl h (x : y : xs) = fst x * snd y + myShapePl h (y : xs)

myShapeMn :: (Double, Double) -> [Point2D] -> Double
myShapeMn h [y] = (-1) * snd y * fst h
myShapeMn h (x : y : xs) = (-1) * snd x * fst y + myShapeMn h (y : xs)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c | 2 * (max (max a b) c) > a + b + c = -1
       | 2 * ((max (max a b) c) ^ 2) > a ^ 2 + b ^ 2 + c ^ 2 = 0
       | 2 * ((max (max a b) c) ^ 2) == a ^ 2 + b ^ 2 + c ^ 2 = 2
       | 2 * ((max (max a b) c) ^ 2) < a ^ 2 + b ^ 2 + c ^ 2 = 1