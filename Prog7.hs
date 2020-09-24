{- ###########
    Matt Minchhoff
    4-30-20
    Homework 7
    #######  -}

module Prog7 where

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

--Function that evaluates a given expression
eval :: Expr -> Int
eval (Val x)= x
eval (Mul x y)= eval x * eval y
eval (Add x y)= eval x + eval y
eval (Sub x y)= eval x - eval y
eval (Div x y)= eval x `div` eval y

--Function that returns the greatest int
maxlit :: Expr -> Int
maxlit (Val x) = x
maxlit (Mul x y) | maxlit x > maxlit y = maxlit x
                 | otherwise = maxlit y
maxlit (Add x y) | maxlit x > maxlit y = maxlit x
                 | otherwise = maxlit y
maxlit (Sub x y) | maxlit x > maxlit y = maxlit x
                 | otherwise = maxlit y
maxlit (Div x y) | maxlit x > maxlit y = maxlit x
                 | otherwise = maxlit y


--Function that evaluates a given expression, catches division by zero
safeeval :: Expr -> Maybe Int
safeeval (Div x (Val 0))= Nothing
safeeval (Div x y) = Just (eval (Div x y))
safeeval expr = Just (eval expr)

--show :: Expr -> String
instance Show Expr where
    show (Val x) = show x
    show (Add x y) = "(" ++ show x ++ "+" ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ "-" ++ show y ++ ")"
    show (Mul x y) = "(" ++ show x ++ "*" ++ show y ++ ")"
    show (Div x y) = "(" ++ show x ++ "/" ++ show y ++ ")"

--Function that adds one to each value in an expression
addone :: Expr -> Expr
addone (Val x) = Val (x+1) 
addone (Add x y) = Add(addone x) (addone y)
addone (Sub x y) = Sub(addone x) (addone y)
addone (Mul x y) = Mul(addone x) (addone y)
addone (Div x y) = Div(addone x) (addone y)

--Function that returns whether a value in list one is in list two
containing :: Eq a => [a] -> [a] -> Bool
containing [] [] = True
containing [] ys = True
containing xs [] = False
containing (x:xs) ys = case elem x ys of
                     True -> containing xs ys
                     False -> False

--Function that finds the sum of squares of negatives
sumSqNeg :: [Int] -> Int
sumSqNeg = foldr (+) 0 . map (^2) . filter (<0)

--Function that returns a list of lengths from two strings
lengths :: [String] -> [Int]
lengths x = map length x

--Functin that applies the function to every element in the list and sums it
total :: (Int -> Int) -> [Int] -> Int
total x = sum . map x

--Function that returns whether a value is in list one and list two(Uses high order functions)
containing' :: Eq a => [a] -> [a] -> Bool
containing' _ [] =False
containing' x y = foldr (&&) True (map elem' x)
  where
    elem' z
      |z `elem` y = True
      |otherwise  = False
