-- 1. Data.List includes a version of Elem for List that works similarly to Elem for Vect. Define it.
{-

data List : (a : Type) -> Type where
     Nil : List a
     (::) : a -> List a -> List a
     
data Elem : (value : a) -> List a -> Type where
     Here : Elem value (value :: xs)
     There : (later : Elem value ys) -> Elem value (x :: ys)
     
-}

-- 2. The following predicate states that a specific value is the last value in a List. Write an isLast function that decides whether a value is the last element in a list

data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value 

noEmptyLast : {x : a} -> Last [] x -> Void
noEmptyLast LastOne impossible

Uninhabited (Last [] x) where
  uninhabited = noEmptyLast


notInNil : Last [] value -> Void
notInNil LastOne impossible
notInNil (LastCons _) impossible

notTheLast : (notEq : (x = value) -> Void) -> Last [x] value -> Void
notTheLast notEq LastOne = notEq Refl
notTheLast notEq (LastCons prf) = notInNil prf




isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInNil
isLast xs value = case isLast xs value of
                                  Yes LastOne => Yes LastOne
                                  Yes (LastCons prf) => Yes (LastCons prf)
                                  No contra => No contra
