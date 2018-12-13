data SplitList : List a -> Type where
     SplitNil : SplitList []
     SplitOne : SplitList [x]
     SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights) 

total
splitList : (input : List a) -> SplitList input
splitList input = splitListHelper input input 
  where
    splitListHelper : List a -> (input : List a) -> SplitList input
    splitListHelper _ [] = SplitNil
    splitListHelper _ [x] = SplitOne
    splitListHelper (_ :: _ :: counter) (item :: items) 
         = case splitListHelper counter items of
                SplitNil => SplitOne
                SplitOne {x} => SplitPair [item] [x]
                SplitPair lefts rights => SplitPair (item :: lefts) rights
    splitListHelper _ items = SplitPair [] items
                                                                

mergeSort : Ord a => (input : List a) -> List a
mergeSort input with (splitList input)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (lefts ++ rights) | (SplitPair lefts rights) = merge (mergeSort lefts) (mergeSort rights)


