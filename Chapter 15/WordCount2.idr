import ProcessLib

record WCData where
       constructor MkWCData
       numLines : Nat
       numWords : Nat
       
data RequestWCData = CountFile String | GetFile String

WCType : RequestWCData -> Type
WCType (CountFile x) = ()
WCType (GetFile x) = Maybe WCData

doCount : String -> WCData
doCount str = let wrds = length (words str)
                  lns = length (lines str) in
                  MkWCData lns wrds
                  
countFile : List (String, WCData) -> (filename : String) -> 
            Process WCType (List (String, WCData)) Sent Sent
countFile xs filename = do Right content <- Action (readFile filename)
                                         | Left err => Pure xs
                           Pure ((filename, (doCount content)) :: xs)
                           
wcService : List (String, WCData) -> Service WCType ()
wcService xs = do msg <- Respond (\res => case res of CountFile x => Pure ()
                                                      GetFile x => Pure (lookup x xs))
                  newList <- case msg of Just (CountFile x) => countFile xs x
                                         _ => Pure xs
                  Loop (wcService newList)

wcClient : Client ()                           
wcClient = do Just pid <- Spawn (wcService [])
                       | Nothing => Action (putStrLn "spawn failed")
              Request pid (CountFile "test.txt")
              Action (putStrLn "Processing file test.txt")
              Just res <- Request pid (GetFile "test.txt")              
                       | Nothing => Action (putStrLn "Error processing file")
              Action (putStrLn ("lines: " ++ show (numLines res)))
              Action (putStrLn ("words: " ++ show (numWords res)))              
                  

