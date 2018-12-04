data Vect : (len : Nat) -> (elem : Type) -> Type where
     Nil  : Vect Z elem
     (::) : elem -> Vect len elem -> Vect (S len) elem

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
      Same : (num : Nat) -> EqNat num num

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)


checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Just (eq) => Just (sameS _ _ eq)
                              Nothing => Nothing

-- checkEqNat uses general Refl constructor 
checkEqNat''' : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat''' Z Z = Just Refl
checkEqNat''' Z (S k) = Nothing
checkEqNat''' (S k) Z = Nothing
checkEqNat''' (S k) (S j) = case checkEqNat''' k j of
                              Just (eq) => Just (cong eq)
                              Nothing => Nothing

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 Just (Same m) => Just input

-- Exercise: Listing 8.3: sameS with case split on eq, and with do notation
checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat' Z Z = Just (Same Z)
checkEqNat' Z (S k) = Nothing
checkEqNat' (S k) Z = Nothing
checkEqNat' (S k) (S j) = case checkEqNat' k j of
                              Just (Same j) => Just (Same (S j))
                              Nothing => Nothing
                              
                              
checkEqNat'' : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat'' Z Z = Just (Same Z)
checkEqNat'' Z (S k) = Nothing
checkEqNat'' (S k) Z = Nothing
checkEqNat'' (S k) (S j) = do (Same j) <- checkEqNat'' k j 
                              Just (Same (S j))

                      
