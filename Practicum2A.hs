{-# LANGUAGE RankNTypes #-}
module Practicum2A where

{-
Name:            Ruben van der Ham
VU-net id:       rhm270
Student number:  2592271
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        https://en.wikipedia.org/wiki/Tower_of_Hanoi
-}

----------------------------
-- Exercise Tower of Hanoi
----------------------------

type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi 0 _ _ _ = []
hanoi n from to temp =
      hanoi (n - 1) from temp to ++
                 [(n, from, to)] ++ hanoi (n-1) temp to from

-- Numbering from 1 (the smallest to n (the largest) with n the disk on the bottom of the stack and 1 the disk on top, respectively
--hanoi 2 "a" "b" "c" == [(1,"a","c"),(2,"a","b"),(1,"c","b")]

-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals  = naturalRecursive 1
naturalRecursive n = n : (naturalRecursive (n+1))

--head(naturals) == 1
--take 3 naturals == [1,2,3]

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = [0,1]++zeroesandones

--take 4 zeroesandones == [0,1,0,1]


-- Exercise 3
threefolds :: [Integer]
threefolds = map (\x -> x * 3) naturals

-- Exercise 4
removeif :: (a -> Bool) -> [a] -> [a]
removeif bool [] = []

removeif bool (x:xs)  = if (bool x) 
     then removeif bool xs
     else x : removeif bool xs

nothreefolds :: [Integer]
nothreefolds = removeif (\x -> mod x 3 == 0) naturals

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds n = map (*n) naturals


-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds z = removeif (\x -> mod x z == 0) naturals

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds n list = removeif (\x -> mod x n == 0) list


-- Exercise 8
eratosthenes :: [Integer]
eratosthenes = eros [2..]
eros (x : xs)  = x: eros (removeif (\y -> mod y x == 0) xs)


-- Exercise 9
fibonacci :: [Integer]
fibonacci = 0: 1 : zipWith (+) (tail fibonacci) fibonacci  


-- -----------------------
-- Exercise Church Numerals
-- -----------------------
-- we need polymorphic types for the Church Numerals 
type ChurchNumeral = forall a . (a -> a) -> a -> a

-- Exercise 1
churchnumeral :: (Eq a, Num a) => a -> ChurchNumeral 
churchnumeral n =
  if   n == 0
  then \s z -> z
  else \s z -> churchnumeral (n - 1) s (s z)

backtointeger :: (Num a) => ChurchNumeral -> a
backtointeger cn = cn (+1) 0

{- Show usage here -}


{-
-- Exercise 2
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality = undefined

-- Exercise 3
successor ::  ChurchNumeral -> ChurchNumeral
successor = undefined
 
-- Exercise 4
successorb :: ChurchNumeral -> ChurchNumeral
successorb = undefined

-- Exercise 5
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  undefined

-- Exercise 6
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition = undefined

multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication = undefined

exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation = undefined

-- Exercise 7
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2  = undefined


-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes = undefined

-- Exercise 2
height :: BinaryTree a -> Integer
height = undefined

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes = undefined

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror = undefined

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten = undefined

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap = undefined

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan = undefined

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan = undefined

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree = undefined

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement = undefined

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert = undefined

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree = undefined

-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove = undefined

-}

