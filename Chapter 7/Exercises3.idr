-- Ex.1 Implement Functor for Expr

data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num) 
              | Abs (Expr num)                                                       
              
eval : (Neg num, Integral num, Abs num) => Expr num -> num              
eval (Val x) = x
eval (Add x y) = eval x + eval y 
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)


Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger
    
Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs

Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs (map func x)

-- Ex.2 Implement Eq and Foldable for Vect

data Vect : (len : Nat) -> (elem : Type) -> Type  where
     Nil : Vect Z elem
     (::) : elem -> Vect len elem -> Vect (S len) elem 

Eq elem => Eq (Vect n elem) where          
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = case x == y of
                                  True => True
                                  False => False


Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: xs) = func x (foldr func init xs)

