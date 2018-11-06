module Practicum1 where

{-
Name:           Ruben van der Ham
VU-net id:      rhm270
Student number: 2592271
Discussed with: --
Remarks:        --
Sources:        https://www.youtube.com/watch?v=02_H3LjqMr8
		https://www.mathsisfun.com/numbers/sigma-calculator.html
		https://stackoverflow.com/questions/1757740/how-does-foldr-work
-}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi x y
    | x > y = x
    | otherwise = y

--maxi 2 3 == 3
--maxi 3 2 == 2



-- Exercise 2
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending a b c d = a < b && b < c && c < d  

--fourAscending 1 2 3 4 == True
--fourAscending 1 2 1 3 == False




-- Exercise 3
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = a==b && b==c && c==d

--fourEqual 1 1 1 1 == True
--fourEqual 199 2 3 1 == False





-- Exercise 4
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent a b c d = ( (a /= b) && (b /= c) && (a /= c) && (a /= d) && (b /= d) && (c /= d))

--fourDifferent 1 2 3 4 == True
--fourDifferent 1 2 2 4 == False 






-- Exercise 5
{-
  threeDifferent :: Integer -> Integer -> Integer -> Bool
  threeDifferent a b c = ( ( a /= b) && (b /= c) )

  This fails when a is equal to c. A is not equal to B and C is not equal to B. So it will return true, although the three are not different.
  Example: 'threeDifferent 1 2 1 == False' will return False
-}




-- Exercise 6
factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x*factorial (x-1)

--factorial 3 == 6
--factorial 4 == 24




-- Exercise 7
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--fib 4 == 3
--fib 6 == 8




-- Exercise 8
-- it is possible to define auxiliary functions
strangeSummation :: Integer -> Integer
strangeSummation  n = 8*n + 28

--strangeSummation 70 == 588
--strangeSummation 50 == 428




-- Exercise 9
lengthList :: [Integer] -> Integer
lengthList []  = 0
lengthList (h:t) = 1 + lengthList t

lengthListAlternative :: [Integer] -> Integer
lengthListAlternative l =
  case l of
    [] -> 0
    (h:t) -> 1 + (lengthListAlternative t)

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs 


--sumList [1,2,3] == 6
--sumList [5,6] == 11





-- Exercise 10
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs  

--doubleList [5,6] == [10,12]
--doubleList [4,6] == [8,12]




-- Exercise 11
myappend :: [a] -> [a] -> [a]
myappend [] [] = []
myappend [] (y:ys)= y : myappend [] ys
myappend (x:xs) y  = x : myappend xs y

--myappend [1,2,3] [5,6] == [1,2,3,5,6]
--myappend [] [1,2,3] == [1,2,3]




-- Exercise 12
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myappend (myreverse(xs)) [x] 

--myreverse [1,2,3,4] == [4,3,2,1]
--myreverse [4,3,3,2,2,1] == [1,2,2,3,3,4]






-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember mem [] = False
mymember mem (x:xs)
  | contains = True
  | otherwise = mymember mem xs
  where contains = x == mem

--mymember 6 [1,2,3,7,8,9] == False
--mymember 4 [7,9,6,4] == True





-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum [] = 0
mysquaresum (x:xs) = x*x + mysquaresum xs


--mysquaresum [1,2,3] == 14
--mysquaresum [1] == 1




-- Exercise 15
range :: Integer -> Integer -> [Integer]
range a b
  | a == b = [a]
  | a > b = []
  | otherwise = a : range (a+1) b

--range 9 7 == []
--range 4 7 == [4,5,6,7]




-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat(xs)  

--myconcat [[1,2,3],[4,5,6]] == [1,2,3,4,5,6]
--myconcat [[9,8,7,6],[5,4,3,2]] == [9,8,7,6,5,4,3,2]





-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert item [] = [item]
insert item (x:xs)
  | item >= x && length xs == 0  = [x, item] -- Last item of the list
  | item >= x && item <= head xs = [x, item]++xs
  | item >= x = [x] ++ insert item xs
  | otherwise  = [item, x] ++ xs          -- First item in the list

--insert 7 [1,2,3,5,7,8,9,12] == [1,2,3,5,7,7,8,9,12]
--insert 7 [] == [7]
--insert 2 [3,4,5,5] == [2,3,4,5,5]

insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort (x:xs) = insert x (insertionsort xs)

--insertionsort [1,98,5,3,7,90,5] == [1,3,5,5,7,90,98]
--insertionsort [9,8,7,76,5,5,4,4,3,22,2] == [2,3,4,4,5,5,7,8,9,22,76]




-- Exercise 18
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort(x:xs) = myappend (quicksort smaller) (x: quicksort larger)
   where smaller = filter (<= x) xs
         larger  = filter (> x) xs

--quicksort [3,9,34,2.8888,4,1,8,0,222] = [0.0,1.0,2.8888,3.0,4.0,8.0,9.0,34.0,222.0]
--quickort[9,8,7,6,5,4,3,2,1] = [1,2,3,4,5,6,7,8,9]




-- Exercise 19
evensB :: [Integer] -> [Integer]
evensB (y) = [x | x <- y, x `mod` 2 == 0]

--evensB [1,2,3,4] == [2,4]
--evensB [1,1,1,1,1,1,1,1,1,1,1,32,77,8889,3] == [32]




-- Exercise 20
mymap :: (a -> b) -> [a] -> [b]
mymap func [] = []
mymap func (x:xs) = func x : mymap func xs 


--mymap round [1.33333,2.33333,3.555555,4.777777,5.88888] == [1,2,4,5,6]
--mymap sqrt [9,66,77] == [3.0,8.12403840463596,8.774964387392123]



-- Exercise 21
twice :: (a -> a) -> a -> a
twice func arg = func (func arg)

--twice (\x -> x+1) 7 == 9
--twice (\x -> x/2) 8 == 2





-- Exercise 22
compose :: (b -> c) -> (a -> b) -> a -> c
compose func1 func2 arg = func1 (func2 arg)

--compose (\x -> x * 5) (\x -> x +4) 7 == 55
--compose (\x -> x + 4) (\x -> x * 5) 7 == 39




-- Exercise 23
mylast :: [a] -> a
mylast list = head (myreverse list)

--mylast [1,2,3,4,5] == 5
--mylast [6,5,4,399] == 399




-- Exercise 24
mylastb :: [a] -> a
mylastb (x)
  | length x <= 1 = head x
  | otherwise = mylastb (drop 1 x) 


--mylastb [1,2,3,4] == 4
--mylastb [99] == 99





-- Exercise 25
myinit, myinitb :: [a] -> [a]
myinit x = take (length x-1) x
myinitb (x:xs)
  | length xs > 0 = [x] ++ myinitb xs 
  | otherwise = []

--myinitb [1,2,3,4] == [1,2,3]
--myinit [1,2,3,4] == [1,2,3]
--myinit [1,2,3,4,5,6,7,8,9,9] == myinitb [1,2,3,4,5,6,7,8,9,9]
--myinit [4] == []
--myinitb [4] == []





-- Exercise 26
mysecondconcat :: [[a]] -> [a]
mysecondconcat list = foldr (++) [] list
--mysecondconcat [[1,2,3],[4,5,6],[7,8,9]] == [1,2,3,4,5,6,7,8,9]

mysecondreverse :: [a] -> [a]
mysecondreverse list = foldr auxAppend [] list

auxAppend :: a -> [a] -> [a]
auxAppend item list = list ++ [item]

--mysecondreverse [1,2,3,4] == [4,3,2,1]
--mysecondreverse [9,8,7,6,5,4,3,2,1] == [1,2,3,4,5,6,7,8,9]





-- Exercise 27
prefix :: [a] -> [[a]]
prefix list = reverse(auxPrefix list)

auxPrefix :: [a] -> [[a]]
auxPrefix [] = [[]]
auxPrefix list =   list : auxPrefix (myinit list)

--prefix [1,2,3] == [[],[1],[1,2],[1,2,3]]
--prefix [1,3,5,7] == [[],[1],[1,3],[1,3,5],[1,3,5,7]]
