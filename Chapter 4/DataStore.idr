module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) -> 
              (items : Vect size String) -> 
              DataStore
              
size : DataStore -> Nat              
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items) 
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
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
                                  Just id => Just (index id store_items ++ "\n", store)

showStrings : (store : DataStore) -> (str : String) -> (vect : Vect (size store) String) -> Maybe (String, DataStore)
showStrings store str vect = Just (generateStringsResult "" str vect, store) where
            generateStringsResult : (resultString : String) -> 
                                    (subStr : String) -> 
                                    (storedStringsVect : Vect k String) ->
                                    String 
            generateStringsResult resultString subStr [] = resultString
            generateStringsResult resultString subStr (x :: xs) 
              = case isInfixOf subStr x of
                        True  => generateStringsResult (x ++ "; "  ++ resultString) subStr xs
                        False => generateStringsResult resultString subStr xs 
                                                   

findSubString : (store : DataStore) -> (str : String) -> Maybe (String, DataStore)
findSubString store str = let vect = items store in
                               showStrings store str vect

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp 
  = case parse inp of
         Nothing  => Just ("Invalid command\n", store)
         Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just Size => Just ("Size " ++ show (size store) ++ "\n", store)
         Just (Search str) => findSubString store str
         Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput


