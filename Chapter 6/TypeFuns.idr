StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (b : Bool) -> StringOrInt b
getStringOrInt False = "Ninety Four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> (StringOrInt isInt) -> String
