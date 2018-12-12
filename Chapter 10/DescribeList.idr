describeListForward : List Int -> String
describeListForward [] = "Empty"
describeListForward (x :: xs) = "Non empty, tail portion: " ++ show xs

{-
describeListEnd : List Int -> String
describeListEnd [] = "Empty"
describeListEnd (xs ++ [x]) = "NonEmpty, initial portion: " ++ show xs
-}

data ListLast : List a -> Type where
     Empty : ListLast [] 
     NonEmpty : (xs : List a) -> a -> ListLast (xs ++ [x])

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non empty, initial portion: " ++ show xs

describeListEnd : List Int -> String
