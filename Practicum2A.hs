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

{- 6 == backtointeger (churchnumeral 6) -}



-- Exercise 2
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality x y = backtointeger x == backtointeger y
 
-- Exercise 3
successor ::  ChurchNumeral -> ChurchNumeral
successor x = churchnumeral ((backtointeger x)+1)

--backtointeger(successor (churchnumeral 6)) == 7


-- Exercise 4
successorb :: ChurchNumeral -> ChurchNumeral
successorb = (\x -> \s -> \z -> x s (s z))

--backtointeger(successorb (churchnumeral 6)) == 7


-- Exercise 5
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  backtointeger(f (churchnumeral n))


-- Exercise 6
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition x y = (\z -> (x z).(y z)) 

multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication x y = (\z -> (x z)) . (\z -> (y z))
{-
exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation x y = \x.y. churchsy z 

-- Exercise 7
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2  = undefined  
-}
-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes (Node x y z) = 1 + numberofnodes x + numberofnodes z
numberofnodes Leaf = 0

exampletree1 = Node Leaf 0 Leaf
exampletree2 = (Node (Node Leaf 1 Leaf ) 0 (Node Leaf 3 Leaf))
exampletree3 = (Node (Node Leaf 1 Leaf) 6 (Node Leaf 8 Leaf))

-- Exercise 2
height :: BinaryTree a -> Integer
height (Node x y z) 
  | (height x > height z) = 1 + height x
  | otherwise =  1 + height z 
height Leaf = 0

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes (Node left val right) = val+ sumnodes left + sumnodes right
sumnodes Leaf = 0 


-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror (Node left val right) = Node (mirror right) val (mirror left)
mirror Leaf = Leaf

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten (Node left val right) = (flatten left) ++ [val] ++ (flatten right)
flatten Leaf = []

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap func (Node left val right)  = Node (treemap func left) (func val) (treemap func right) 
treemap func Leaf = Leaf


-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan ref (Node left val right) = ref < val && (smallerthan ref left) && (smallerthan ref right)
smallerthan ref Leaf = True

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan ref (Node left val right) = ref > val && (largerthan ref left) && (largerthan ref right)
largerthan ref Leaf = True

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree (Node left val right) = (largerthan val left) && (smallerthan val right) && isbinarysearchtree left && isbinarysearchtree right
isbinarysearchtree Leaf = True 

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement ref (Node left val right)
  | ref == val = True
  | ref < val = iselement ref left
  | otherwise = iselement ref right
iselement ref Leaf = False

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert new Leaf = Node Leaf new Leaf
insert new (Node left val right)
  | new == val = (Node left val right)
  | new < val =  (Node (insert new left) val (right))
  | otherwise = (Node (left) val (insert new right))

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree (x:xs) = createbst xs (Node Leaf x Leaf)
createbinarysearchtree [] = Leaf


createbst :: (Ord a, Eq a) => [a] -> BinaryTree a -> BinaryTree a
createbst (x:xs) (Node left val right) = createbst xs (insert x (Node left val right))
createbst [] (Node left val right) = (Node left val right)
--isbinarysearchtree (createbinarysearchtree [1,2,3,4,5,6,7,8,9]) == True

-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove x (Leaf) = Leaf
remove x (Node left val right)
  | x > val = (Node left val (remove x right))  --remove from right if removal value higher than value of the node
  | x < val = (Node (remove x left) val right) --remove from left if removal value lower than value of the node
  | (left == Leaf) && (right == Leaf) =  Leaf --On node without children return leaf
  | right == Leaf = left                       -- When right is a leaf replace this node by left
  | left == Leaf = right                       --  when left is a leaf replace this node by right
  | otherwise = Node (left) (successorval) (remove (successorval) (right))  --When both children get successor node val remove it and make current val successor val
  where  successornode = getsuccessornode right
         successorval = (nodeval successornode)
       

getsuccessornode :: (Ord a, Eq a) => BinaryTree a -> BinaryTree a
getsuccessornode (Node left val right)
  | getsuccessornode left == Leaf = (Node Leaf val Leaf) -- On last node return that node
  | otherwise = getsuccessornode (left) --Try to go most to the left to get smallest from the right of the inital node
getsuccessornode (Leaf) = Leaf


nodeval :: BinaryTree a -> a
nodeval (Node x val z) = val 

--isbinarysearchtree (remove 8 exampletree3)

