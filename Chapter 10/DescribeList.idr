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


-- listLast is a covering function for the ListLast view (converts a value into ListLast)
-- covering func should have the same name as view type but with lower case letter
total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty ys y => NonEmpty (x :: ys) y

{-
describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)
-}

-- the with blocks: syntax for extended pattern matching

describeListEnd : List Int -> String
describeListEnd input with (listLast input)
  describeListEnd [] | Empty = "Empty"
  describeListEnd (xs ++ [x]) | (NonEmpty xs y) = "Non empty, initial portion: " ++ show xs
  
-- Example: reversing a list using a view
-- traverse a list from right to left, inspecting the last element first. Use listLast

myReverse : List a -> List a    
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NonEmpty xs y) = y :: (myReverse xs)



