import ProcessLib

record WCData where
       constructor MkWCData 
       nlines : Nat
       nwords : Nat
       
data WCDataRequest = CountFile String | GetFile String

WCType : WCDataRequest -> Type 
WCType (CountFile x) = ()
WCType (GetFile x) = Maybe WCData

doCount : String -> WCData
doCount x = let lns = length (lines x)
                wrds = length (words x) in
                MkWCData lns wrds
                
countFile : (files : List (String, WCData)) -> (filename : String) -> 
            Process WCType (List (String, WCData)) Sent Sent
countFile files filename = do Right content <- Action (readFile filename)
                                            | Left err => Pure files
                              Pure ((filename, doCount content) :: files)

serviceCount : List (String, WCData) -> Service WCType ()
serviceCount xs = do msg <- Respond (\msg => case msg of (CountFile x) => Pure ()
                                                         (GetFile x) => Pure (lookup x xs))
                     newFiles <- case msg of (Just (GetFile x)) => countFile xs x
                                             _ => Pure xs
                     Loop (serviceCount newFiles)

mainClient : Client ()
mainClient = do Just pid <- Spawn (serviceCount [])
                         | Nothing => Action (putStrLn "spawn failed")
                Request pid (CountFile "test.txt")
                Action (putStrLn "Processing file test.txt")
                Just res <- Request pid (GetFile "test.txt")
                         | Nothing => Action (putStrLn "file error")
                Action (putStrLn ("lines: " ++ show (nlines res)) )
                Action (putStrLn ("words: " ++ show (nwords res)) )
                              
