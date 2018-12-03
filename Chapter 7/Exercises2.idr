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

-- 6 + 3 * 12

expr1 : Expr Integer
expr1 = Add (Val 6) (Mul (Val 3) (Val 12)) 

res1 : Integer
res1 = eval expr1

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger
    
Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs

-- Exercise 1 : Implement Show for the Expr type

Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = show x ++ "+" ++ show y
  show (Sub x y) = show x ++ "-" ++ show y
  show (Mul x y) = "(" ++ show x ++ "*" ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ "/" ++ show y ++ ")"
  show (Abs x) = show x

-- Exercise 2 : Implement Eq for the Expr type. Expressions should be considered equal if their evaluation is equal.

(Eq ty, Abs ty, Integral ty, Neg ty) => Eq (Expr ty) where
  (==) x y = case eval x == eval y of
                       True => True
                       False => False 

-- Exercise 3 : Implement Cast to allow conversions from Expr num to any appropriately constrained type num.

(Num num, Abs num, Integral num, Neg num) => Cast (Expr num) num where
  cast expr = (eval expr)
