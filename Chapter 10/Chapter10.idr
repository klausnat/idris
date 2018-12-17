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
  
  

