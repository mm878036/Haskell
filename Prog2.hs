{- ##################
   Matt Minchhoff.
   Homework 2.
################## -}

module Prog2 where

--If any two of the input are the same the output is False
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z 
  |(x==y)&& (y==z)&&(x==z) = False
  |(y==z)|| (x==z)||(x==y) = False
  |otherwise               = True 

--If any two of the input are the same the output is False
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent w x y z 
  | (threeDifferent w x y)==False =False
  | (threeDifferent w y z)==False =False
  | (threeDifferent x y z)==False =False
  | otherwise =True  

--The sum of numbers n to 1 are sumed together
sum' :: Integer -> Integer
sum' n
  |n == 1 = 1
  |n > 1  = n + sum'(n-1) 

--The sum of a ascii string is calculated
asciisum :: String -> Integer
asciisum [] = 0
asciisum (x:xs) = asciisum xs + toInteger(fromEnum x)

--Finds the square root of an integer
integerSqrt :: Integer -> Integer
integerSqrt x = floor(sqrt(fromInteger (x)))

--Finds the maximum of three integers
maxOfThree :: Integer -> Integer -> Integer -> Integer
maxOfThree x y z
  | x>=y && x>=z=x
  |y>=z         =y
  |otherwise    =z

--Finds the minimum of three Integers
minOfThree :: Integer -> Integer -> Integer -> Integer
minOfThree x y z
  |x<=y && x<=z =x
  |y<=z         =y
  |otherwise    =z

--Finds the middle of three integers
middleOfThree :: Integer -> Integer -> Integer -> Integer
middleOfThree x y z
  |x<=y && x>=z || x>=y && x<=z =x
  |y<=x && y>=z || y>=x && y<=z =y
  |otherwise                    =z

--Returns a triple of integers into a tuple of increasing order
orderTriple :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
orderTriple (x,y,z)= (minOfThree x y z, middleOfThree x y z, maxOfThree x y z)

--Switches the first and fourth list of Chars, and leaves the middle two alone
swap :: (Char,Char,Char,Char) -> (Char,Char,Char,Char)
swap (w,x,y,z) = (z,x,y,w)

--Negates numbers for negateTwoDigits
negateInt :: Integer -> Integer
negateInt x
  |(x < (-9)) && (x > (-100)) = x + (-x) + (-x)
  |(x > 9) && (x < 100)       = x +(-x) + (-x)
  |otherwise                  =x

--Negates all two digit numbers
negateTwoDigits :: [Integer] -> [Integer]
negateTwoDigits x = [negateInt t | t <- x]

--Returns an Int from a list of Ints
matches :: Integer -> [Integer] -> [Integer]
matches x xs= [t|t <- xs, x==t]

--Uses the matches function to return a True/False statement if the Int is present
element :: Integer -> [Integer] -> Bool
element x xs
  |length(matches x xs)> 0 =True
  |otherwise               =False
