import Data.Vect

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse {n = S k} (x :: xs) = let result = myReverse xs ++ [x] in
                                    rewrite plusCommutative 1 k in result


myReverse' : Vect n elem -> Vect n elem
myReverse' [] = []
myReverse' (x :: xs) = reverseProof (myReverse' xs ++ [x])
   where
     reverseProof : Vect (len + 1) elem -> Vect (S len) elem
     reverseProof {len} result = rewrite plusCommutative 1 len in result
      
append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append {m} [] ys = rewrite plusZeroRightNeutral m in ys
append (x :: xs) ys = append_xs (x :: append xs ys) where
                        append_xs : Vect (S (m + len)) elem -> Vect (plus m (S len)) elem
                        append_xs {m} {len}  xs = rewrite sym (plusSuccRightSucc m len) in xs
                       
                       
