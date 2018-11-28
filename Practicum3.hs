module Practicum3 where

{-
Name:           <Name and family name>
VU-net id:      <VU-net id>
Student number: <Student number>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <in case you used sources such as books or webpages
                 please mention them here>
-}

-- Exercises Arithmetical Expressions
data IntExp  = Lit Int | Add IntExp IntExp | Mul IntExp IntExp
  deriving Show

showintexp :: IntExp -> String
showintexp = undefined

evalintexp :: IntExp -> Int
evalintexp = undefined

-- Exercises Combinatory Logic
data Term = S | K | I | App Term Term

instance Show Term where
  show a = showterm a

showterm :: Term -> String
showterm = undefined

isredex :: Term -> Bool
isredex = undefined

isnormalform :: Term -> Bool
isnormalform = undefined

headstep :: Term -> Term
headstep = undefined

-- Exercises Equational Specifications
data Thing = Undefined1
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt = undefined

-- 
data I = Undefined2
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s = undefined

p :: I -> I
p = undefined

