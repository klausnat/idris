-- SnocList type parameterized over the equivalent List
data SnocList : List a -> Type where
     Empty : SnocList [] 
     Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelper : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelper snoc [] {input} = rewrite appendNilRightNeutral input in snoc
snocListHelper snoc [x] = Snoc snoc
snocListHelper snoc {input} (x :: xs) = ?xx (snocListHelper (Snoc snoc {x}) xs)

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelper Empty xs

myReverseHelper : (input : List a) -> (snoc : SnocList input) -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

myReverse : List a -> List a
myReverse xs = myReverseHelper xs (snocList xs)



