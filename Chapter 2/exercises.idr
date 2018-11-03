-- what are the types of the following values?

z : (String, String, String)
z = ("A","B","C")

y : List String
y = ["A", "B", "C"]

x : ((Char, String), Char)
x = (('A', "B"), 'C')

-- 2 Write a palindrome function

palindrome2 : String -> Bool
palindrome2 x = if x == reverse x then True else False

-- 3 Modify the palindrome function so that it's not case sensitive
palindrome3 : String -> Bool
palindrome3 x = if z == Prelude.Strings.reverse z then True else False where
               z = toLower x

-- 4 Modify the palindrome function so that it only returns True for strings longer than 10 chars
palindrome4 : String -> Bool
palindrome4 x = if (z == Prelude.Strings.reverse z) && (length x > 10) then True else False where
               z = toLower x  
                
-- 5 Pass string length as an argument                            
palindrome5 : String -> Nat -> Bool
palindrome5 x n  = if (z == Prelude.Strings.reverse z) && (length x > n) then True else False where
               z = toLower x 

-- 6 Write a counts function of type String -> (Nat, Nat)
counts : String -> (Nat,Nat)
counts str = ((length $ words str),length str )

-- 7 Write a top_ten function of type Ord a => List a -> List a that returns the ten largest values in a list.

top_ten : Ord a => List a -> List a
top_ten xs = (take 10) $ reverse $ sort xs

-- 8 Write an over_length function that returns the number of strings in the list longer than the given number of characters

over_length : Nat -> List String -> Nat
over_length n xs = let listOfLengths = map length xs 
                       sortedList = filter (>n) listOfLengths in
                       length sortedList
                       
