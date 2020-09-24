{- ##########
    Matt Minchhoff
    4-23-2020
    Homework 6
    ######## -}

module Prog6 where

data Set= Set [Int]
        | EmptySet
    deriving Show

--Helper function for equal to sort
member :: Int -> Set -> Bool
member n (Set []) = member n (EmptySet)
member n (EmptySet) = False
member n (Set (x:xs)) = x == n || (member n (Set xs))

--Function that returns whether two sets are equal
equal :: Set -> Set -> Bool
equal EmptySet EmptySet = True
equal (Set [x]) EmptySet = False
equal EmptySet (Set [x]) = False
equal (Set [x]) (Set []) = False
equal (Set []) (Set [x]) = False
equal (Set []) (Set []) = True
equal (Set [x]) (Set [y]) = x == y
equal (Set x) (Set y) = length [1 | z <- x, member z (Set y)] == length y

--Function that removes an item from a given set
saferemove :: Int -> Set -> Maybe Set
saferemove x (EmptySet) = Nothing
saferemove x (Set xs)
  | member x (Set xs) == False = Nothing
  |otherwise = Just (toEmptySet(Set([y|y<-xs,y/=x])))

--Helper Functinon for saferemove
toEmptySet :: Set -> Set
toEmptySet (EmptySet) = (EmptySet)
toEmptySet (Set []) = (EmptySet)
toEmptySet (Set n) = (Set n)

--Function that returns the union of two sets
union :: Set -> Set -> Set
union EmptySet EmptySet = EmptySet
union EmptySet y = y
union x EmptySet = x
union(Set []) (Set [])= EmptySet
union (Set []) (Set y) = Set y
union (Set x) (Set []) = Set x
--union (Set (x:xs)) (Set (y:ys)) = case x == y of
  --True -> sum x ( union (Set xs) (Set ys) )
  --False -> Set ( setToList ( sum x ( sum y ( union (Set xs) (Set ys)))))

--Heleper for union
--setToList :: Set -> [Char]
--setToList (Set x) = x

data Tree = Leaf Int
          | Node Tree Int Tree

--Function that orders a tree in a preorder traversal
preorder :: Tree -> [Int]
preorder (Leaf x) = [x]
preorder (Node le y ri) = y : (preorder le) ++ (preorder ri)

--Function that orders a tree in a postorder traversal
postorder :: Tree -> [Int]
postorder (Leaf x) = [x]
postorder (Node le y ri) =  (postorder le) ++ (postorder ri) ++ [y] 

--Function that counts the number of 0s in a tree
countZeros :: Tree -> Int
countZeros x = length[y | y <- (preorder x), y==0]

--Functin that returns the number of leaves in a tree
countLeaves :: Tree -> Int
countLeaves (Leaf x) = 1
countLeaves (Node le x ri) = countLeaves le + countLeaves ri

--Functino that returns the number of interior nodes in a tree
countInteriorNodes :: Tree -> Int
countInteriorNodes (Leaf x) = 0
countInteriorNodes (Node le x ri) = 1 + (countInteriorNodes le) + (countInteriorNodes ri)

--Function that returns the depth of a tree
depth :: Tree -> Int
depth (Leaf x) = 0
depth (Node le x ri) = x + depth le + depth ri

--Function that tests if the left and right subtree have an equal number of values
balanced :: Tree -> Bool
balanced (Leaf x) = True
balanced (Node le x ri) = or [help le == (help ri-1),help ri == (help le -1),help le == help ri]

--helper function for balanced
help:: Tree -> Int
help (Leaf x) = 1
help (Node n1 x n2) = help n1 + help n2
