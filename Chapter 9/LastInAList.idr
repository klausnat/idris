-- The following predicate states that that a specific value is the last value in a List

data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value
     
-- proof of Last [1,2,3] 3:

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

-- Write an isLast function that decides whether a value is the last element in a List.

notInNil : Last [] value -> Void
notInNil LastOne impossible
notInNil (LastCons _) impossible

headDoesntMatter : (prf : Last ys value) -> Last (y :: ys) value
headDoesntMatter LastOne = LastCons LastOne
headDoesntMatter (LastCons prf) = LastCons (LastCons prf)

notLastNotCons : (contra : (y = value) -> Void) -> (contra1 : Last [] value -> Void) -> Last [y] value -> Void
notLastNotCons contra contra1 LastOne = contra Refl
notLastNotCons contra contra1 (LastCons prf) = contra1 prf

definitelyNot : (contra : Last (x :: xs) value -> Void) -> Last (y :: (x :: xs)) value -> Void
definitelyNot contra (LastCons prf) = contra prf

isHeadMatter : DecEq a => (ys : List a) -> (y : a) -> (value : a) -> (contra : Last ys value -> Void) -> Dec (Last (y :: ys) value)
isHeadMatter [] y value contra = case decEq y value of
                                      Yes Refl => Yes LastOne
                                      No contra1 => No (notLastNotCons contra1 contra)
isHeadMatter (x :: xs) y value contra = No (definitelyNot contra)

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInNil
isLast (y :: ys) value = case isLast ys value of
                                Yes prf => Yes (headDoesntMatter prf)
                                No contra => isHeadMatter ys y value contra

