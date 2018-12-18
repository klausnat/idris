data ListLast : (xs : List a) -> Type where
     Empty : ListLast []
     NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

total 
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty zs z => NonEmpty (x :: zs) z

describeListEnd : (xs : List Int) -> String
describeListEnd xs with (listLast xs)
  describeListEnd [] | Empty = "Empty"
  describeListEnd (ys ++ [x]) | (NonEmpty ys x) = "non-empty, init part: " ++ show ys
  
myReverse : List a -> List a  
myReverse xs with (listLast xs)
  myReverse [] | Empty = []
  myReverse (ys ++ [x]) | (NonEmpty ys x) = x :: myReverse ys

----------------------------------------------------------------------
data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)
     
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z [] = Exact []
takeN Z (x :: xs) = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) with (takeN k xs)
  takeN (S k) (x :: xs) | Fewer = Fewer
  takeN (S k) (x :: (n_xs ++ rest)) | (Exact n_xs) = Exact (x :: n_xs)

                
groupByN : (n : Nat) -> (xs : List a) -> List (List a)          
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

---------------------------------------------------------------------
data SplitList : (List a) -> Type where
     SplitNil : SplitList []
     SplitOne : SplitList [x]
     SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

total          
splitList : (input : List a) -> SplitList input     
splitList input = splitListHelper input input 
     where
       splitListHelper : (counter : List a) -> (input : List a) -> SplitList input
       splitListHelper _ [] = SplitNil
       splitListHelper _ [x] = SplitOne
       splitListHelper (_ :: _ :: counter) (item :: input) 
         = case splitListHelper counter input of
                SplitNil => SplitOne 
                SplitOne {x} => SplitPair [item] [x]
                SplitPair lefts rights => SplitPair (item :: lefts) rights
       splitListHelper _ rest = SplitPair [] rest         

mergeSort : Ord a => List a -> List a
mergeSort input with (splitList input)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (lefts ++ rights) | (SplitPair lefts rights) = merge (mergeSort lefts) (mergeSort rights)
  
  

