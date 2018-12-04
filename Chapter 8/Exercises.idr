-- 1. Implement the func that states that if you add the same value onto the front of equal lists, the resulting lists are also equal

same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

-- Second solution

same_cons' : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons' prf = cong prf

-- 2. Implement the following func, which states that if two values, x and y, are equal, and two lists xs, ys are equal, the the two lists x :: xs and y :: ys must also be equal

same_lists : {xs : List a} -> {ys : List a} -> xs = ys -> x = y -> x :: xs = y :: ys
same_lists Refl Refl = Refl

-- 3. Define a type ThreeEq that expresses that three values must be equal

data ThreeEq : a -> b -> c -> Type where
     SameThree : x -> ThreeEq x x x

-- 4. Implement the following function which uses ThreeEq

congForThreeEq : (x,y,z : Nat) -> (eq : ThreeEq x y z) -> ThreeEq (S x) (S y) (S z)
congForThreeEq a a a (SameThree a) = SameThree (S a)

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x y z x1 = congForThreeEq x y z x1
