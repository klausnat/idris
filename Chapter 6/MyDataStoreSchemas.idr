module Main

{- кол-во подглядываний: (data DataStore), addItem,                     -}

import Data.Vect

infixr 5 .+.

data Schema = SString | SInt | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Integer
SchemaType (schemal .+. schemar) = (SchemaType schemal, SchemaType schemar)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

data Command : Schema -> Type where
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Quit : Command schema

getQuotedHelper : List Char -> Maybe (String, String)
getQuotedHelper ('"' :: xs) = let (result, rest) = Strings.span (/= '"') (pack xs) in
                                  case unpack rest of
                                       ('"' :: rest') => Just (result, ltrim (pack rest'))
                                       _              => Nothing
getQuotedHelper _ = Nothing 

getQuoted : String -> Maybe (String, String)
getQuoted x = getQuotedHelper (unpack x)


parseSchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseSchema SString x = case getQuoted x of 
                             Nothing => Nothing
                             Just (str,rest) => case rest of 
                                                     "" => Just str
                                                     _  => Nothing 
            
parseSchema SInt x = case all isDigit (unpack x) of
                          True => Just (cast x)
                          False => Nothing
parseSchema (y .+. z) x = case Strings.span (/= ' ') x of
                               (a,"") => Nothing
                               (a,b) => case parseSchema y a of
                                             Just sch1 => case parseSchema z (ltrim b) of 
                                                               Nothing => Nothing
                                                               Just sch2 => Just (sch1, sch2)
                                             Nothing => Nothing


parse : (schema : Schema) -> (String, String) -> Maybe (Command schema)
parse schema (a, b) = case a of "Add" => case parseSchema schema (ltrim b) of
                                              Nothing => Nothing
                                              Just res => Just (Add res)
                                "Get" => case all isDigit (unpack (ltrim b)) of
                                              True => Just (Get (cast b))
                                              False => Nothing 
                                "Quit" => Just Quit
                                _      => Nothing

parseCommand : (schema : Schema) -> String -> Maybe (Command schema)
parseCommand schema x = case Strings.span (/= ' ') x of
                      (com, arg) => parse schema (com, arg)
                      _          => Nothing

addItem : (store : DataStore) -> (newitem : SchemaType (schema store)) -> (String, DataStore)
addItem (MkData schema size items) newitem = ("ID: " ++ cast size ++ "\n", MkData schema (S size) (addIt items newitem)) where
                                      addIt : Vect len (SchemaType schema) -> (SchemaType schema) -> Vect (S len) (SchemaType schema)
                                      addIt [] newit = [newit]
                                      addIt (x :: xs) newit = x :: (addIt xs newit)

display : (schema : Schema) -> SchemaType schema -> String
display SString x = x
display SInt x = show x
display (y .+. z) (a, b) = show (display y a, display z b)

getItem : (store : DataStore) -> (num : Integer) -> Maybe (String, DataStore)
getItem store@(MkData schema size items) num = case integerToFin num size of
                                       Nothing  => Just ("Index Out of Bounds \n", store)
                                       Just res => Just (display schema (index res items) ++ "\n", store)

processInput : (store : DataStore) -> (input : String) -> Maybe (String, DataStore)
processInput store input = case parseCommand (schema store) input of
                                Nothing      => Just ("Invalid command \n", store)
                                Just (Add str) => Just (addItem store str)
                                Just (Get num) => getItem store num
                                Just Quit => Nothing

main : IO ()
main = replWith (MkData (SString .+. SInt .+. SString) Z []) "Command: " processInput


