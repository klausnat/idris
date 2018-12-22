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

-- 3. Define a Face data type that represents the faces of a coin: heads or tails. 
data Face = Heads | Tails

randoms : Int -> Stream Int
randoms seed = let seed' = 3245 * seed + 2349876 in
                   (shiftR seed' 2) :: randoms seed'

getFace : Int -> Face
getFace x = if (div x 2) * 2 /= x then Tails else Heads

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count xs = take count (map getFace xs)

-- test not working. getFace not forced in result list

--4. Define a function to calculate a square root of a Double 
square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = let next = getNext number approx in next :: square_root_approx number next 
        where
         getNext : (nmb : Double) -> (appr : Double) -> Double
         getNext nmb appr = (appr + (nmb / appr)) / 2
         
-- first approximation of a square root should be within a desired bound
square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: xs) = value
square_root_bound (S k) number bound approxs = ?square_root_bound_rhs_2





