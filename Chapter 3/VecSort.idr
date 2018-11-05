import Data.Vect

insert : Ord elem =>  (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs 
                              else y :: insert x xs

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

||| Exercises : define your own version of functions length, reverse and map

total my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

total my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

total my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = (f x) :: (my_map f xs)

total my_map_vect : (a -> b) -> Vect n a -> Vect n b
my_map_vect f [] = []
my_map_vect f (x :: xs) = (f x) :: (my_map_vect f xs)

{-
||| Matrix operations and their types

matrix : Vect rows (Vect cols elem)

addMatrix : Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType)

||| Matrix multiplication
||| amount of elems in row of first matrix should be equal to 
||| amount of elements in column in second matrix

multMatrix : Num numType => 
             Vect n (Vect m numType) ->
             Vect m (Vect p numType) ->
             Vect n (Vect p numType)

-}
                                                    
||| Transposing a matrix

||| Reimplement transposeMat using zipWith instead of transposeHelper

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transposeHelper vect matr = zipWith (\e, es => e :: es) vect matr

-- transposeHelper [] [] = []
-- ransposeHelper (x :: xs) (y :: ys) = (x :: y) :: ransposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         transposeHelper x xsTrans

||| Implement addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)

sumVects : Num a => Vect m a -> Vect m a -> Vect m a
sumVects [] [] = []
sumVects (x :: xs) (y :: ys) = (x + y) :: (sumVects xs ys)

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix vec1 vec2 = zipWith (sumVects) vec1 vec2
