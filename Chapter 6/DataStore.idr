module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

teststore : DataStore              
teststore = (MkData (SString .+. SInt) 1 [("Answer",42)])




addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = MkData schema _ (addToData store) 
  where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items


data Command : Schema -> Type where 
      Add : SchemaType schema -> Command schema
      Get : Integer -> Command schema
      Quit : Command schema

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "Add" arg = Just (Add (?xx arg))
parseCommand schema "Get" val = case all isDigit (unpack val) of
                              False => Nothing 
                              True => Just (Get (cast val))
parseCommand schema "Quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd,args) => parseCommand schema cmd (ltrim args)


display : SchemaType schema -> String

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                             case integerToFin pos (size store) of
                                  Nothing => Just ("Out of range \n", store)
                                  Just id => Just ((display store id pos store_items) (index id (items store)) ++ "\n", store)


showStrings : (resultString : String) -> (subStr : String) -> (storedStringsVect : Vect k (Nat,String)) -> String 
showStrings resultString subStr [] = resultString
showStrings resultString subStr myvect@((i,s) :: xs) 
              = case Strings.isInfixOf subStr s of
                        True  => showStrings (show i ++ ": "  ++ s ++ "; " ++ resultString) subStr xs
                        False => showStrings resultString subStr xs 
                                                   


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp 
  = case parse (schema store) inp of
         Nothing  => Just ("Invalid command\n", store)
         Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput

