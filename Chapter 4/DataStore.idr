module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) -> 
              (items : Vect size (Nat,String)) -> 
              DataStore
              
size : DataStore -> Nat              
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) (Nat,String)
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items) 
  where
    addToData : Vect old (Nat,String) -> Vect (S old) (Nat,String)
    addToData [] = [(size, newitem)]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String 
             | Get Integer
             | Search String
             | Quit
             | Size

parseCommand : String -> String -> Maybe Command
parseCommand "Add" arg = Just (Add arg)
parseCommand "Get" val = case all isDigit (unpack val) of
                              False => Nothing 
                              True => Just (Get (cast val))
parseCommand "Quit" "" = Just Quit
parseCommand "Search" str = Just (Search str)
parseCommand "Size" "" = Just Size
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd,args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                             case integerToFin pos (size store) of
                                  Nothing => Just ("Out of range \n", store)
                                  Just id => let (i, s) = index id store_items in Just (show i ++ ", " ++ s ++ "\n", store)

showStrings : (resultString : String) -> (subStr : String) -> (storedStringsVect : Vect k (Nat,String)) -> String 
showStrings resultString subStr [] = resultString
showStrings resultString subStr myvect@((i,s) :: xs) 
              = case Strings.isInfixOf subStr s of
                        True  => showStrings (show i ++ ": "  ++ s ++ "; " ++ resultString) subStr xs
                        False => showStrings resultString subStr xs 
                                                   

findSubString : (store : DataStore) -> (str : String) -> String
findSubString store str = let vect = items store in
                               showStrings "" str vect

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp 
  = case parse inp of
         Nothing  => Just ("Invalid command\n", store)
         Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just Size => Just ("Size " ++ show (size store) ++ "\n", store)
         Just (Search str) => Just (findSubString store str ++ "\n", store)
         Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput


