-- 1. Write an every_other function that produces a new Stream from every second element of an input Stream.

every_other : Stream Type -> Stream (Stream Type)
every_other (value1 :: value2 :: values) = (repeat value2) :: every_other values
-- test is not working, task is not clear to me

-- 2. Write an implementation of Functor for InfList

data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

Functor InfList where
  map func (value :: xs) = func value :: map func xs

-- defining coungFrom as as an infinite list
countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

 
getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs
