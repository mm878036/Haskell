{- #############
  Matt Minchhoff
  Homework 5.
  4-14-2020
  ############ -}

module Prog5 where
import Data.Char

--Function that reverses a list, mimics that of the reverse function
reverse' :: [a] -> [a]
reverse' x =
  case (x) of
  [] -> []
  (x:[]) -> [x]
  (x:xs) -> reverse' xs ++ [x]

--Function that tests if a list is a palindrome
isPalindrome :: String -> Bool
isPalindrome x =
  case x of
  [] -> True
  (x:[]) -> True
  (x:xs)-> ((x:xs) == reverse' (x:xs))

type TimeStamp = (Int,Int,Int)

--Functino that takes two timestamp types and sees which one is longer
isLonger :: TimeStamp -> TimeStamp -> Int
isLonger (hr1,min1,sec1)(hr2,min2,sec2) 
  |hr1>hr2 = (-1)
  |hr2>hr1 = 1
  |min1>min2 = (-1)
  |min2>min1 = 1
  |sec1>sec2 = (-1)
  |sec2>sec1 = 1
  |otherwise = 0

--Funciton that finds the total amount of seconds in a video
totalSeconds :: TimeStamp -> Int
totalSeconds (TimeStamp(hr,min,sec)) = sec + (min*60) + (hr*3600)

--Function that finds whether a timestamp has valid values of hr min and sec
isValid :: TimeStamp -> Bool
isValid (TimeStamp(hr,min,sec)) = (hr>=0) && (min>=0 && min <=59) &&(sec>=0 && sec<=59)

--Function that returns a string depection of the timestamp type
time2Str :: TimeStamp -> String
time2Str (TimeStamp(hr,min,sec))
  |isValid(TimeStamp(hr,min,sec)) == True = show hr ++":" ++ show min ":" ++ show sec
  |isValid(TimeStamp(hr,min,sec))||sec<10||min<10||hr<10 = "0" ++ show hr ++ ":" ++ "0" ++show min ++ ":" ++ "0"++ show sec
  |isValid(TimeStamp(hr,min,sec))==False = "invalid input"
  |otherwise = show hr ++":" ++ show min ++":" ++ show sec

--Function that searches for a number in a list and returns each number that preceeds it in value
safeFindBefore :: Int -> [Int] -> Maybe [Int]
safeFindBefore x = [(xs,x)|xs<-x, xs<x]

data Set = Set [Int]
	  | EmptySet
	 deriving Show
--Function that checks if an item is in a set and returns a bool value
member :: Int -> Set -> Bool
member (x:Set:xs)
  |x `elem` xs = True
  |otherwise = False 

--Function that returns the number of elements in a set
size :: Set -> Int
size [] = 0
size (_:xs) = 1 +size xs

--Function that inserts an item in a set
ins :: Int -> Set -> Set
ins _ = error "undefined"
