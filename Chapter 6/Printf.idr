module Main

data Format = Number Format
            | Str Format
            | Lit String Format
            | End

%name Format fmt, fmt1, fmt2

exampleFormat : Format                          
exampleFormat = Str (Lit " = " (Number End)) 

-- calculating a type of printf from a Format specifier
PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit x fmt) = PrintfType fmt
PrintfType End = String

-- helper function for printf building a string from a Format specifier
printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit x fmt) acc = printfFmt fmt (acc ++ x)
printfFmt End acc = acc

-- top-level definition of printf, with a conversion from String to Format

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

