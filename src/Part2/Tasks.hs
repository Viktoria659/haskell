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
replaceVar varName t (Variable(x)) | x == varName = t
                                   | otherwise = Variable x
replaceVar varName t (IntConstant(x)) = IntConstant x
replaceVar varName t  (BinaryTerm op lhv rhv) = BinaryTerm op (replaceVar varName t lhv) (replaceVar varName t rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Plus (IntConstant a) (IntConstant b)) = IntConstant (a + b)
evaluate (BinaryTerm Minus (IntConstant a) (IntConstant b)) = IntConstant (a - b)
evaluate (BinaryTerm Times (IntConstant a) (IntConstant b)) = IntConstant (a * b)
evaluate (BinaryTerm op (BinaryTerm op1 a1 b1) (BinaryTerm op2 a2 b2)) =
  evaluate (BinaryTerm op (evaluate (BinaryTerm op1 a1 b1)) (evaluate (BinaryTerm op2 a2 b2)))
evaluate term = term
