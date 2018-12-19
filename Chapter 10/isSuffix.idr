-- SnocList type parameterized over the equivalent List
data SnocList : List a -> Type where
     Empty : SnocList [] 
     Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelper : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelper snoc [] {input} = rewrite appendNilRightNeutral input in snoc
snocListHelper snoc [x] = Snoc snoc
snocListHelper snoc {input} (x :: xs) = rewrite (appendAssociative input [x] xs) in (snocListHelper (Snoc snoc {x}) xs)

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelper Empty xs

isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1)
  isSuffix [] input2 | Empty = True
  isSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    isSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) 
               = if x == y then isSuffix xs ys | xsrec | ysrec 
                           else False




