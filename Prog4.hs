{- ################
  Matt Minchhoff
  Homework 4.
  4-2-2020
  ############## -}
module Prog4 where
import Data.Char

--Funciton that returns a list of a number and it doubled
doubleAll:: [Int] -> [(Int,Int)]
doubleAll [] = []
doubleAll (x:xs) =  (x,x*2) : doubleAll xs 

--Function that multiples the last n numbers in a list, n is the first #
productLastPart:: Int ->[Int]->Int
productLastPart x[]=1
productLastPart x xs = productLastPart(x-1)(init xs) *last xs

--Function that works like the built in function init
init' :: [Int] -> [Int]
init' [] = error "empty"
init' [x] = []
init' (x:xs) = x : (init' xs)

--Function that lowercases every odd letter in a string
lowerOddLetters :: String -> String
lowerOddLetters [] =[]
lowerOddLetters [x]= [toLower x]
lowerOddLetters(x:y:z)= toLower x:y:lowerOddLetters z

--Function that works like the installed function replicate
replicate' :: Int -> Char -> String
replicate' x y
  |x==0 =[]
  |otherwise =y: replicate' (x-1)y

--Function that uses insertion sort to sort a list
iSort' :: [(Int,String)] -> [(Int,String)]
iSort' [] =[]
iSort' (x:xs) = insert' x (iSort' xs)

--helper for iSort'
insert' :: (Int,String)->[(Int,String)]->[(Int,String)]
insert'(a,b) [] = [(a,b)]
insert' n(x:xs)
  |n<= x =n:x:xs
  |n> x=x:insert' n xs

--Function that lowers the 1st character in a string
lowerFirstCharacter :: String -> String
lowerFirstCharacter (x:xs) = if isUpper x then (toLower x:xs) else  lowerFirstCharacter(x:xs)

--Function that returns the 2nd word in a string of 3
middleWord :: String-> String
middleWord (x:xs)
  |elem ' ' (x:xs)== False = (x:xs)
  |x== ' ' = middleWord(reverse xs)
  |otherwise = middleWord xs

--Function that lowercases the first upper letter in a string
lowerFirstLetter:: String->String
lowerFirstLetter [] =[]
lowerFirstLetter(x:xs)
  |isUpper x = toLower x : xs ++ lowerFirstLetter[]
  |otherwise = x: lowerFirstLetter xs

--Function that lowercases the first 2 upper letters in a string
lowerFirstTwoLetters :: String -> String
lowerFirstTwoLetters xs= lowerFirstLetter(lowerFirstLetter xs)
