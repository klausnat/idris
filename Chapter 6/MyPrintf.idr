module main 

data Format = Lit String Format
            | Num Format
            | Str Format
            | End 

%name Format format

FormatToType : (fmt : Format) -> Type
FormatToType (Lit x format) = FormatToType format
FormatToType (Num format) = Integer -> FormatToType format
FormatToType (Str format) = String -> FormatToType format
FormatToType End = String


stringToFormat : (str : List Char) -> Format
stringToFormat ('%' :: 'd' :: chars) = Num (stringToFormat chars)
stringToFormat ('%' :: 's' :: chars) = Str (stringToFormat chars)
stringToFormat (c :: chars) = case stringToFormat chars of 
                                   Lit str fmt => Lit (strCons c str) fmt
                                   fmt => Lit (strCons c "") fmt
stringToFormat [] = End

printfH : (fmt : Format)  -> (acc : String) -> FormatToType fmt
printfH (Lit x format) acc = printfH format (acc ++ x)
printfH (Num format) acc = \i => printfH format (acc ++ show i)
printfH (Str format) acc = \str => printfH format (acc ++ str)
printfH End acc = acc

printf : (str : String) -> FormatToType (stringToFormat (unpack str)) 
printf str = printfH _ ""
