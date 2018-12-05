import Data.Vect

-- 1. Using plusZeroRightNeutral and plusSuccRightSucc, write your own version of plusCommutes

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = ?s

-- 2. make myReverse efficient

reversseProof_xs : (x : a) -> (acc : Vect n1 a) -> (xs : Vect len a) -> Vect ((S n1) + len) a -> Vect (plus n1 (S len)) a
reversseProof_xs {n1} {len} x acc xs result = rewrite (sym (plusSuccRightSucc n1 len)) in result

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' {n} acc [] = rewrite plusZeroRightNeutral n in acc
        reverse' acc (x :: xs) = reversseProof_xs x acc xs (reverse' (x :: acc) xs)
