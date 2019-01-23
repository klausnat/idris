import ProcessLib

record WCData where
       constructor MkWCData
       lineCount : Nat
       wordCount : Nat
       
doCount : (content : String) -> WCData
doCount content = let lcount = length (lines content)
                      wcount = length (words content) in
                      MkWCData lcount wcount
              
data WC = CountFile String | GetData String

WCType : WC -> Type 
WCType (CountFile x) = ()
WCType (GetData x) = Maybe WCData

countFile : (files : List (String, WCData)) -> (fname : String) -> 
            Process WCType (List (String, WCData)) Sent Sent
countFile files fname = 
  do Right content <- Action (readFile fname)
            | Left err => Pure files
     Action  (putStrLn ("Counting complete for " ++ fname))
     Pure ((fname, doCount content) :: files)

wcService : List (String, WCData) -> Service WCType ()
wcService loaded 
  = do msg <- Respond (\msg => case msg of 
                                    CountFile fname => Pure ()
                                    GetData fname => Pure (lookup fname loaded)
                      )
       newLoaded <- case msg of
                         Just (CountFile fname) => countFile loaded fname
                         _ => Pure loaded
       Loop (wcService newLoaded)
  
procMain : Client ()
procMain = do Just wc <- Spawn (wcService [])            
                      | Nothing => Action (putStrLn "spawn failed")
              Action (putStrLn "Counting test.txt")
              Request wc (CountFile "test.txt")
              
              Action (putStrLn "Processing")
              Just wcdata <- Request wc (GetData "test.txt")
                          | Nothing => Action (putStrLn "File Error")
              Action (putStrLn ("Words: " ++ show (wordCount wcdata)))
              Action (putStrLn ("Lines: " ++ show (lineCount wcdata)))              
