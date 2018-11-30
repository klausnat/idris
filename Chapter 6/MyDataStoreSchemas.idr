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
     Set : Schema -> Command schema
     Quit : Command schema

getQuotedHelper : List Char -> Maybe (String, String)
getQuotedHelper ('"' :: xs) = let (result, rest) = Strings.span (/= '"') (pack xs) in
                                  case unpack rest of
                                       ('"' :: rest') => Just (result, ltrim (pack rest'))
                                       _              => Nothing
getQuotedHelper _ = Nothing 

getQuoted : String -> Maybe (String, String)
getQuoted x = getQuotedHelper (unpack x)

parsePrefix : (schema : Schema) -> (input : String) -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted input
            
parsePrefix SInt input = case span isDigit input of
                              (a,b)  => Just (cast a, ltrim b)
                              ("",_) => Nothing

parsePrefix (y .+. z) input = case parsePrefix y input of 
                                   Just (resY,input') => case parsePrefix z input' of
                                                             Just (resZ,input'') => Just ((resY, resZ),input'')
                                                             Nothing => Nothing
                                   Nothing => Nothing
                                                

parseSchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseSchema schema input = case parsePrefix schema input of
                                Just (res, "") => Just res
                                Just (res, _)  => Nothing
                                Nothing        => Nothing

stringToSchHelper : String -> (Maybe Schema)
stringToSchHelper "String" = Just SString
stringToSchHelper "Int" = Just SInt
stringToSchHelper _ = Nothing

stringToSchema : String -> Maybe Schema
stringToSchema "" = Nothing
stringToSchema x  = case Strings.span (/= ' ') x of
                         (a,"") => case stringToSchHelper a of 
                                        Just res => Just res
                                        Nothing => Nothing
                         (a,b)  => case (stringToSchHelper a) of 
                                        Nothing => Nothing
                                        Just resA => case stringToSchema (ltrim b) of
                                                         Nothing => Nothing
                                                         Just resB => Just (resA .+. resB)
parse : (schema : Schema) -> (String, String) -> Maybe (Command schema)
parse schema (a, b) = case a of "Add" => case parseSchema schema (ltrim b) of
                                              Nothing => Nothing
                                              Just res => Just (Add res)
                                "Get" => case all isDigit (unpack (ltrim b)) of
                                              True => Just (Get (cast b))
                                              False => Nothing 
                                "Set" => case stringToSchema (ltrim b) of
                                              Just sch => Just (Set sch)
                                              Nothing => Nothing
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
display (y .+. z) (a, b) = (display y a) ++ ", " ++ (display z b)

getItem : (store : DataStore) -> (num : Integer) -> Maybe (String, DataStore)
getItem store@(MkData schema size items) num = case integerToFin num size of
                                       Nothing  => Just ("Index Out of Bounds \n", store)
                                       Just res => Just (display schema (index res items) ++ "\n", store)

setSchema : (store : DataStore) -> (schema : Schema) -> Maybe (String, DataStore) 
setSchema store schema = case length (items store) of
                              Z => Just ("OK \n", MkData schema Z [])
                              _ => Just ("There are entries in the store, can't set new schema! \n", store)

processInput : (store : DataStore) -> (input : String) -> Maybe (String, DataStore)
processInput store input = case parseCommand (schema store) input of
                                Nothing      => Just ("Invalid command \n", store)
                                Just (Add str) => Just (addItem store str)
                                Just (Get num) => getItem store num
                                Just (Set sche) => setSchema store sche
                                Just Quit => Nothing

main : IO ()
main = replWith (MkData SString Z []) "Command: " processInput

{- пользователь должен вводить схему так : Set String Int Int String -}
