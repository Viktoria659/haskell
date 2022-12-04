module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
(|+|) :: Term -> Term -> Term
(|+|) x y = BinaryTerm Plus x y

infixl 6 |-|
(|-|) :: Term -> Term -> Term
(|-|) x y = BinaryTerm Minus x y

infixl 7 |*|
(|*|) :: Term -> Term -> Term
(|*|) x y = BinaryTerm Times x y

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar = notImplementedYet

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate = notImplementedYet
