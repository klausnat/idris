module FCTypes 

StringOrInt : Bool -> Type
StringOrInt x = case x of 
                       True  => Int
                       False => String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x = case x of 
                          True => 88
                          False => "Eighty Eight"
                          
valToString : (x : Bool) -> StringOrInt x -> String
valToString x val = case x of 
                           True  => cast val
                           False => val                          

