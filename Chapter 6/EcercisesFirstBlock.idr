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

-- 2 Extend printf to support formatting directives for Char and Double
-- see file Printf.idr

-- 3 You could implement a vector as nested pairs, 
-- with the nesting calculated from the length, as in this example
-- TupleVect 0 ty = ()
-- TupleVect 1 ty = (ty, ())
-- TupleVect 2 ty = (ty, (ty, ()))
-- Define a type-level function TupleVect, that implements this behaviour
-- remember to start with the type of TupleVect

TupleVect : Nat -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, (TupleVect k x))

test : TupleVect 4 Nat
test = (1,2,3,4,())
