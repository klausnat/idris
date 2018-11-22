module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SChar = Char
SchemaType SInt = Int

SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

teststore : DataStore              
teststore = (MkData (SString .+. SInt) 2 [("Answer",42), ("Question", 3)])


addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = MkData schema _ (addToData store) 
  where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items


data Command : Schema -> Type where 
      SetSchema : (newSchema : Schema) -> Command schema
      Add : SchemaType schema -> Command schema
      Get : Integer -> Command schema
      Quit : Command schema

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                    [] => Just SString
                                    _ => do xs_sch <- parseSchema xs
                                            Just (SString .+. xs_sch)

parseSchema ("Int" :: xs) = case xs of
                                 [] => Just SInt
                                 _ => do xs_sch <- parseSchema xs
                                         Just (SInt .+. xs_sch)
parseSchema ("Char" :: xs) = case xs of
                                 [] => Just SChar
                                 _ => do xs_sch <- parseSchema xs
                                         Just (SChar .+. xs_sch)
                                                                                      
parseSchema _ = Nothing

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input) 
            where
               getQuoted : List Char -> Maybe (String, String)
               getQuoted ('"' :: xs) = case span (/= '"') xs of
                                              (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                              _                     => Nothing
               getQuoted _ = Nothing


parsePrefix SInt input = case span isDigit input of
                              ("",rest) => Nothing
                              (num,rest)  => Just (cast num, ltrim rest)

parsePrefix SChar input = let (x :: rest) = unpack input in Just (x, ltrim (pack rest))

parsePrefix (schema1 .+. schema2) input = do (l_val, input') <- parsePrefix schema1 input
                                             (r_val, input'') <- parsePrefix schema2 input'
                                             Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> (input : String) -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of 
                                  Just (res, "") => Just res
                                  Just _         => Nothing 
                                  Nothing        => Nothing            

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "Schema" rest = do schemaok <- parseSchema (words rest)
                                       Just (SetSchema schemaok)
                                         
parseCommand schema "Add" rest = do restok <- (parseBySchema schema rest)
                                    Just (Add restok)
parseCommand schema "Get" val = case all isDigit (unpack val) of
                              False => Nothing 
                              True => Just (Get (cast val))
parseCommand schema "Quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd,args) => parseCommand schema cmd (ltrim args)


display : SchemaType schema -> String
display {schema = SString}  x = show x
display {schema = SInt}  x = show x
display {schema = SChar} x = show x
display {schema = (y .+. z)}  (xleft, xright) = display xleft ++ ", " ++ display xright

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                             case integerToFin pos (size store) of
                                  Nothing => Just ("Out of range \n", store)
                                  Just id => Just (display (index id (items store)) ++ "\n", store)


showStrings : (resultString : String) -> (subStr : String) -> (storedStringsVect : Vect k (Nat,String)) -> String 
showStrings resultString subStr [] = resultString
showStrings resultString subStr myvect@((i,s) :: xs) 
              = case Strings.isInfixOf subStr s of
                        True  => showStrings (show i ++ ": "  ++ s ++ "; " ++ resultString) subStr xs
                        False => showStrings resultString subStr xs 
                                                   

setSchema : (store : DataStore) -> Schema -> Maybe (DataStore)
setSchema store schema = case size store of 
                              Z => Just (MkData schema _ [])
                              S k => Nothing 

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp 
  = case parse (schema store) inp of
         Nothing  => Just ("Invalid command\n", store)
         
         Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (SetSchema schema') => case setSchema store schema' of
                                          Nothing => Just ("Can't update schema \n", store)
                                          Just store' => Just ("OK\n", store')
         Just (Get pos) => getEntry pos store
         Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput

maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = case x of 
                    Nothing => Nothing
                    Just x_val => case y of 
                                       Nothing => Nothing
                                       Just y_val => Just (x_val + y_val)

-- (>>=) : Monad m => m a -> (a -> m b) -> m b
maybeAdd' : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd' x y = x >>= \x_val => 
                y >>= \y_val =>
                Just (x_val + y_val)
                
maybeAdd'' : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd'' x y = do x_val <- x
                    y_val <- y
                    Just (x_val + y_val)
