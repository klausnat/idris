-- import Data.Vect

data Vect : (len : Nat) -> (a : Type) -> Type where
     Nil  : Vect Z a
     (::) : (x : a) -> (xs : Vect len a) -> Vect (S len) a

-- 1. Implement the following functions :

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra: (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void  
headUnequal contra Refl = contra Refl
                                        
tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl




-- 2. Implement DecEq for Vect. Begin with the following implementation header : 


DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq xs ys of
                                 Yes Refl => case decEq x y of
                                                 Yes Refl => Yes Refl
                                                 No cntr => No (headUnequal cntr)
                                 No contra => No (tailUnequal contra)

