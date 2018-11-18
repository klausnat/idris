StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (b : Bool) -> StringOrInt b
getStringOrInt False = "Ninety Four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> (StringOrInt isInt) -> String
valToString False x = trim x
valToString True x = cast x

valToString' : (isInt : Bool) -> (case isInt of
                                       False => String
                                       True => Int) -> String
valToString' False x = trim x
valToString' True x = cast x

AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)

-- for any numeric type
AdderType' : (numargs : Nat) -> Type -> Type
AdderType' Z x = x
AdderType' (S k) x = (next : x) -> AdderType' k x

adder' : Num numType => (numargs : Nat) -> numType -> AdderType' numargs numType 
adder' Z acc = acc
adder' (S k) acc = \next => adder' k (next + acc)



 
