{- ##############
   Matt Minchhoff
   3-3-20
   Homework 3.
############## -}
module Prog3 where
import Data.Char

--Takes a list of ints and returns a pair with the original number and the
--the number doubled
doubleAll :: [Int] -> [(Int,Int)]
doubleAll x = [(m,2*m)|m<-x]

--Returns a product of the last n numbers in the list
productLastPart :: Int -> [Int] -> Int
productLastPart x xs = product(take x(reverse(xs)))

--Similar to the init function, accepts a list and returns the list without
--the last item
init' :: [Int] -> [Int]
init' x
  | x == []  =[]
  |otherwise = reverse(tail(reverse x))

--Takes a string arguement and returns true if it is nesting at least a pair
--of parentheses
nestedParens :: String -> Bool
nestedParens "" = True
nestedParens "(" = False
nestedParens ")" = False
nestedParen(x:xs)
  |x == '(' && last xs == ')' = nestedParens (init xs)
  |otherwise = False 

--Generates a list of integers 
triads :: Int -> [(Int,Int,Int)]
triads xs = [(x,y,z) | x<-[1..xs], y<-[1..xs],z<-[1..xs],x^2+y^2==z^2]

--helper functions for pushRight
numStr :: String -> Int
numStr x = sum[1|n<-x]

space:: Int -> String
space y = replicate y ' '

--Takes a string and an int and forms a string of length x by putting spaces
pushRight :: String -> Int -> String
pushRight x y = space(y-numStr x) ++ x

--Makes the first letter in a string lower case, otherwise keeps it the same
lowerFirstCharacter :: String -> String
lowerFirstCharacter []  = []
lowerFirstCharacter (x:xs) = (toLower x) : xs

--helpers for middleWord
word:: String-> [Int]
word sA= [x|x<-[0..((length sA)-1)], isSpace(sA !! x)]

firstSpace::String->[Int]->Int
firstSpace wordX x = head x

secondSpace :: String-> [Int] ->Int
secondSpace wordX y = head(reverse y)

deleteWord :: String->String
deleteWord w = [w !! x | x<-[0..length w],x>(firstSpace w(word w))&&x<(secondSpace w(word w))]

--Displays the middle word in a set of three
middleWord :: String -> String
middleWord z = deleteWord(z)

--Makes the first letter in a string lower, will continue until it finds 
--one that will change
lowerFirstLetter :: String -> String
lowerFirstLetter xs = [toLower x | x<-xs,x >= 'A' && x<='Z']

--
lowerFirstTwoLetters :: String -> String
lowerFirstTwoLetters xs = [toLower x | x<-xs,x>='A' && x<= 'Z']
