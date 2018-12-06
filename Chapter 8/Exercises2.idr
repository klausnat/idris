import Data.Vect

-- 1. Using plusZeroRightNeutral and plusSuccRightSucc, write your own version of plusCommutes


myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite (myPlusCommutes k m) in plusSuccRightSucc m k

-- 2. make myReverse efficient

reversseProof_xs : Vect ((S n1) + len) a -> Vect (plus n1 (S len)) a
reversseProof_xs {n1} {len} result = rewrite (sym (plusSuccRightSucc n1 len)) in result

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' {n} acc [] = rewrite plusZeroRightNeutral n in acc
        reverse' acc (x :: xs) = reversseProof_xs (reverse' (x :: acc) xs)
