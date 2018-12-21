data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

{-
Inf : Type -> Type

Delay : (value : ty) -> Inf ty
Force : (computation : Inf ty) -> ty 
-}                  

-- defining coungFrom as as an infinite list
countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

 
getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs
 
{- 
data Stream : Type -> Type where
     (::) : (value : elem) -> Inf (Stream elem) -> Stream elem

repeat : elem -> Stream elem
take :  (n : Nat) -> (xs : Stream elem) -> List elem
iterate : (f : elem -> elem) -> (x : elem) -> Stream elem  
-}

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbs [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals

label : List a -> List (Integer,a)
label = labelWith (iterate (+ 1) 0) 


