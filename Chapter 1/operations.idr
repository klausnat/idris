module operations

import Data.Vect

||| suggest possible operations that would satisfy given input and output types (could be more than one answer in each case)

op1 : Vect n elem -> Vect n elem
op1 [] = []
op1 (x :: xs) = x :: (op1 xs)

op2 : Vect n elem -> Vect (n * 2) elem
op2 [] = []
op2 (x :: xs) = x :: x :: op2 xs

op3 : Vect (1 + n) elem -> Vect n elem
op3 (x :: xs) = xs

||| Assume that Bounded n represents a number between zero and n - 1
op4 : Vect 4 elem -> elem
op4 (x :: xs) = x
