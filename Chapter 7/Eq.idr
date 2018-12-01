data Matter = Liquid | Solid | Gas

Eq Matter where
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) Solid Solid = True
  (==) _ _ = False
  
  
occurrences : Matter -> List Matter -> Nat  
occurrences x [] = 0
occurrences x (y :: xs) = case x == y of
                               True => 1 + occurrences x xs
                               False => occurrences x xs

  
