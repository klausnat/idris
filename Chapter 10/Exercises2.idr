-- 1. Write a getValues function that returns a list of all values in a DataStore. It should have the following type:

import DataStore

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty 

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues x with (storeView x)
  getValues x | SNil = []
  getValues (addToStore (key,value) store) | (SAdd rec) = value :: getValues store | rec


            
