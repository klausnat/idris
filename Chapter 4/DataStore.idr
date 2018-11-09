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
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "Add" arg = Just (Add arg)
parseCommand "Get" val = case all isDigit (unpack val) of
                              False => Nothing 
                              True => Just (Get (cast val))
parseCommand "Quit" "" = Just (Quit)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd,args) => parseCommand cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput x y = ?processInput_rhs

main : IO ()
main = replWith (MkData _ []) "Command: " processInput


