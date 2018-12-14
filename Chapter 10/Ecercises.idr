-- 1. The TakeN view allows traversal of a list several elements at a time

data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

total          
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) with (takeN k xs)
  takeN (S k) (x :: xs) | Fewer = Fewer
  takeN (S k) (x :: (n_xs ++ rest)) | (Exact n_xs) = Exact (x :: n_xs)
   

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest


-- 2. Use TakeN to define a function that splits a list  into two halves by calculating it's length
halves : List a -> (List a, List a)
halves xs = halvesHelper (div (length xs) 2) xs 
  where
    halvesHelper : (n : Nat) -> (xs : List a) -> (List a, List a)
    halvesHelper n xs with (takeN n xs)
      halvesHelper n xs | Fewer = ([],xs)
      halvesHelper n (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)
