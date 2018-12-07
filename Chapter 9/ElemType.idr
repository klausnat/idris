data Vect : (len : Nat) -> (elem : Type) -> Type where
     Nil : Vect Z elem
     (::) : (x : elem) -> Vect len elem -> Vect (S len) elem

data Elem : (value : elem) -> (xs : Vect len elem) -> Type where
     Here : Elem value (value :: ys)
     There : (later : Elem x xs) -> Elem x (y :: xs) 

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notHere : (value = x) -> Void) -> (notThere : Elem value xs -> Void) -> Elem value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: y) = case decEq value x of
                             Yes Refl => Yes Here
                             No notHere => case isElem value y of
                                                Yes prf => Yes (There prf)
                                                No notThere => No (notInTail notHere notThere)
