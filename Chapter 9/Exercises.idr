-- 1. Data.List includes a version of Elem for List that works similarly to Elem for Vect. Define it.
{-

data List : (a : Type) -> Type where
     Nil : List a
     (::) : a -> List a -> List a
     
data Elem : (value : a) -> List a -> Type where
     Here : Elem value (value :: xs)
     There : (later : Elem value ys) -> Elem value (x :: ys)
     
-}

-- 2. The following predicate states that a specific value is the last value in a List. Write an isLast function

data Last : List a -> a -> Type where

