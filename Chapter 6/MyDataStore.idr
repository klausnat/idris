module Main

{- кол-во подглядываний: |                    -}

import Data.Vect

record DataStore where
       constructor MkData
       size : Nat
       items : Vect size String

data Command = Add String | Get Integer | Quit

parse : (String, String) -> Maybe Command
parse (a, b) = case a of "Add" => Just (Add (ltrim b))
                         "Get" => case all isDigit (unpack (ltrim b)) of
                                       True => Just (Get (cast b))
                                       False => Nothing 
                         "Quit" => Just Quit
                         _      => Nothing

parseCommand : String -> Maybe Command
parseCommand x = case Strings.span (/= ' ') x of
                      (com, arg) => parse (com, arg)
                      _          => Nothing

addItem : (store : DataStore) -> (newitem : String) -> (String, DataStore)
addItem (MkData size items) newitem = ("ID: " ++ cast size ++ "\n", MkData (S size) (addIt items newitem)) where
                                      addIt : Vect len String -> String -> Vect (S len) String
                                      addIt [] newit = [newit]
                                      addIt (x :: xs) newit = x :: (addIt xs newit)

getItem : (store : DataStore) -> (num : Integer) -> Maybe (String, DataStore)
getItem store@(MkData size items) num = case integerToFin num size of
                                       Nothing  => Just ("Index Out of Bounds \n", store)
                                       Just res => Just ((index res items) ++ "\n", store)

processInput : (store : DataStore) -> (input : String) -> Maybe (String, DataStore)
processInput store input = case parseCommand input of
                                Nothing      => Just ("Invalid command \n", store)
                                Just (Add str) => Just (addItem store str)
                                Just (Get num) => getItem store num
                                Just Quit => Nothing
                    
main : IO ()
main = replWith (MkData Z []) "Command: " processInput
