import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views


-- 1. Implement an equalSuffix function using the SnocList view defined in Data.List.Views
-- should return the maximum equal suffix of the two input lists

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix input1 input2 with (snocList input1)
  equalSuffix [] input2 | Empty = []
  equalSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    equalSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) 
                  = if x == y then equalSuffix xs ys | xsrec | ysrec ++ [x]
                              else []

-- 2. Implement mergeSort for vectors, using the SplitRec view defined in Data.Vect.Views

mergeSort : Ord a => Vect n a -> Vect n a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (xs ++ ys) | (SplitRecPair lrec rrec) 
             = merge (mergeSort xs | lrec) (mergeSort ys | rrec)


-- 3. Write a toBinary function that converts a Nat to a String containing a binary representation of the Nat. You should use the HalfRec view defined in Data.Nat.Views

toBinary : Nat -> String
toBinary n with (halfRec n)
  toBinary Z | HalfRecZ = ""
  toBinary (x + x) | (HalfRecEven rec) = (toBinary x | rec) ++ "0"
  toBinary (S (x + x)) | (HalfRecOdd rec) = (toBinary x | rec) ++ "1"

-- 4. Write a palindrome function that returns whether a list is the same when traversed forwards and backwards, using the VList view defined in Data.List.Views

palindrome : Eq a => (input : List a) -> Bool
palindrome input with (vList input)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | (VCons rec) 
              = if x == y then palindrome xs | rec
                          else False 

