module Main
import Data.Vect

||| 1. An n x m matrix can be represented by nested vectors of Double. 
||| Define a type synonim: Matrix : Nat -> Nat -> Type 

Matrix : Nat -> Nat -> Type -> Type
Matrix n m reqT = Vect n (Vect m reqT)

testMatrix : Matrix 2 3 Int
testMatrix = [[0,0,0],
	      [0,0,0]]

-- Vect 2 (Vect 3 Double)
