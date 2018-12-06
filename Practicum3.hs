module Practicum3 where

{-
Name:           Ruben van der Ham
VU-net id:      rhm270
Student number: 25912271
Discussed with: 
Remarks:        
Sources:        First few duckduckgo hits on combinatory logic
                the specification Toy of the slides, and of Example 6.4 of the course notes.
-}

-- Exercises Arithmetical Expressions
data IntExp  = Lit Int | Add IntExp IntExp | Mul IntExp IntExp
  deriving Show

showintexp :: IntExp -> String
showintexp (Lit int) = show int
showintexp (Add (Lit int1) (Lit int2)) = "("++show int1++"+"++show int2++")" 
showintexp (Mul (Lit int1) (Lit int2)) = "("++show int1++"*"++show int2++")" 


evalintexp :: IntExp -> Int
evalintexp (Lit int) = int
evalintexp (Add (Lit int1) (Lit int2)) = int1 + int2
evalintexp (Mul (Lit int1) (Lit int2)) = int1 * int2

-- Exercises Combinatory Logic
data Term = S | K | I | App Term Term

instance Show Term where
  show a = showterm a

showterm :: Term -> String
showterm (App x y) = "("++showterm x ++ showterm y ++ ")"
showterm S = "S"
showterm K = "K"
showterm I = "I"

--showterm (App (App I S) K) == "((IS)K)"


isredex :: Term -> Bool
isredex (App I y) = True
isredex (App(App(App S x)y)z) = True
isredex (App(App K x)y) = True
isredex anythingelse = False

isnormalform :: Term -> Bool
isnormalform I = True
isnormalform K = True
isnormalform S = True
isnormalform (App K y) = isnormalform y
isnormalform (App S y) = isnormalform y  
isnormalform (App (App S x) y) = isnormalform x && isnormalform y
isnormalform anythingelse = False

 
headstep :: Term -> Term
headstep (App I y) = y
headstep (App (App K x) y) = x
headstep (App (App (App S x) y) z) = (App (App x z) (App y z))
headstep anythingelse = anythingelse 


-- Exercises Equational Specifications
data Thing = B | C | D
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt B = D
nxt C = B
nxt D = C


data I = Undefined2
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s = undefined

p :: I -> I
p = undefined

