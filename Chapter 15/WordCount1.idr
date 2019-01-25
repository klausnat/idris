import ProcessLib1

record WCData where
       constructor MkWCData
       wordCount : Nat
       lineCount : Nat
       
doCount : (content : String) -> WCData       
doCount content = let words = length $ words content
                      lines = length $ lines content in
                      MkWCData words lines

data WC = CountFile String | GetData String

WCType : WC -> Type
WCType (CountFile x) = ()
WCType (GetData x) = Maybe WCData

countFile : (files : List (String, WCData)) -> (fname : String) -> 
            Process WCType (List (String, WCData)) Sent Sent
countFile files fname = do Right content <- Action (readFile fname)
                                     | Left err =>  Pure files
                           Action (putStrLn $ "counting complete for " ++ fname)
                           Pure ((fname, doCount content) :: files)

wcService : List (String, WCData) -> Service WCType ()
wcService initList = do msg <- Respond (\msg => case msg of CountFile x => Pure ()
                                                            GetData x => Pure (lookup x initList))
                        newLoaded <- case msg of (Just (CountFile x)) => countFile initList x
                                                 _ => Pure initList
                        Loop (wcService newLoaded)
                        
procMain : Client ()
procMain = do Just pid <- Spawn (wcService [])
                  | Nothing => Action (putStrLn "spawn failed")
              Action (putStrLn "Counting test.txt")
              Request pid (CountFile "test.txt")
              
              Action (putStrLn "Processing test.txt")
              Just wcdata <- Request pid (GetData "test.txt")
                          | Nothing => Action (putStrLn "File error")    
              Action (putStrLn ("lines: " ++ show (lineCount wcdata)))
              Action (putStrLn ("words: " ++ show (wordCount wcdata)))              
