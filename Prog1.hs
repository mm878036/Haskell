{- ######################
	Matt Minchhoff.
	2-11-2020
	Homework 1.
####################### -}

module Prog1 where

--This function returns whether a floating value is positive or not.
isPositive :: Float -> Bool
isPositive x 
  | x >= 0    = True
  | otherwise =  False

--This function returns whether this function is evenly divisible by 5
dividesEvenlyByFive :: Integer -> Bool
dividesEvenlyByFive x 
  | x `mod` 5 == 0 = True
  | otherwise      = False

--This function returns the middle value of 3 integers.
middle :: Integer -> Integer -> Integer -> Integer
middle x y z  
  | x>y && x<z     = x
  | y>x && y<z     = y
  | z>x && z<y     = z
  | x<y && x>z     = x
  | y<x && y>z     = y
  | z<x && z>y     = z

--This function computes the nor gate of two boolean expressions
nor :: Bool -> Bool -> Bool
nor x y 
  | x == False && y== False  = True
  | otherwise                = False 

--This function calculates the area of a triangle
triangleArea:: Integer -> Integer -> Float
triangleArea x y = fromInteger(x*y)/2

--This function calculates the ceiling of a number and returns it as a float
ceilingDecimal :: Float -> Float
ceilingDecimal x  = fromInteger(ceiling x)

--This function returns a letter grade from the input of a integer value
letterGrade:: Integer -> String
letterGrade x   
  | x >=93           = "A"
  | x >=90 && x<93   = "A-"
  | x >=87 && x<90   = "B+"
  | x >=83 && x<87   = "B"
  | x >=80 && x<83   = "B-"
  | x >=77 && x<80   = "C+"
  | x >=73 && x<77   = "C"
  | x >=70 && x<73   = "C-"
  | x >=67 && x<70   = "D+"
  | x >=63 && x<67   = "D"
  | x >=60 && x<63   = "D-"
  | x <60            = "F"

--This function calculates the average value of three integers
averageThree:: Integer -> Integer -> Integer -> Float
averageThree x y z = fromInteger(x+y+z)/3

--This function calculates how many integers are above the average value
howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage x y z 
  | x > round(averageThree x y z) && y > round(averageThree x y z) =2
  | x > round(averageThree x y z)                                  =1
  | y > round(averageThree x y z) && z > round(averageThree x y z) =2
  | y > round(averageThree x y z)                                  =1
  | z > round(averageThree x y z)                                  =1
  | x > round(averageThree x y z) && z > round(averageThree x y z) =2
  | otherwise                                                      =0

--This function returns a value from the first three values and whether
--they are in the threshold
howManyWithinThreshold:: Integer -> Integer -> Integer -> Float -> Integer
howManyWithinThreshold w x y z
  |(averageThree w x y)-z <  fromInteger(x)&& (averageThree w x y)+z > fromInteger(x) && (averageThree w x y)-z < fromInteger(y)&& (averageThree w x y)+z > fromInteger(y)&& (averageThree w x y)-z<fromInteger(w) && (averageThree w x y) +z > fromInteger(w) =3
  |(averageThree w x y)-z <  fromInteger(x)&& (averageThree w x y)+z > fromInteger(x) && (averageThree w x y)-z < fromInteger(y)&& (averageThree w x y)+z > fromInteger(y)|| (averageThree w x y)-z>fromInteger(w) && (averageThree w x y) +z < fromInteger(w) =2
  |(averageThree w x y)-z <  fromInteger(x)&& (averageThree w x y)+z > fromInteger(x) && (averageThree w x y)-z < fromInteger(y)|| (averageThree w x y)+z > fromInteger(y)&& (averageThree w x y)-z>fromInteger(w) || (averageThree w x y) +z < fromInteger(w) =1
  |(averageThree w x y)-z <  fromInteger(x)&& (averageThree w x y)+z > fromInteger(x) && (averageThree w x y)-z < fromInteger(y)|| (averageThree w x y)+z < fromInteger(y)&& (averageThree w x y)-z<fromInteger(w) && (averageThree w x y) +z > fromInteger(w) =2
  |(averageThree w x y)-z <  fromInteger(x)&& (averageThree w x y)+z > fromInteger(x) && (averageThree w x y)-z < fromInteger(y)&& (averageThree w x y)+z > fromInteger(y)&& (averageThree w x y)-z<fromInteger(w) && (averageThree w x y) +z > fromInteger(w) =2
  |(averageThree w x y)-z >  fromInteger(x)|| (averageThree w x y)+z < fromInteger(x) && (averageThree w x y)-z < fromInteger(y)&& (averageThree w x y)+z > fromInteger(y)&& (averageThree w x y)-z>fromInteger(w) || (averageThree w x y) +z < fromInteger(w) =1
  |(averageThree w x y)-z >  fromInteger(x)|| (averageThree w x y)+z < fromInteger(x) && (averageThree w x y)-z > fromInteger(y)|| (averageThree w x y)+z < fromInteger(y)&& (averageThree w x y)-z<fromInteger(w) && (averageThree w x y) +z > fromInteger(w) =1
  |otherwise =0
