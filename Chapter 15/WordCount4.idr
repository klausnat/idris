import ProcessLib

record WCData where
       constructor MkWcData
       numLines : Nat
       numWords : Nat
       
doCount : (content : String) -> WCData       
doCount content = let numl = length (lines content)
                      numw = length (words content) in
                      MkWcData numl numw
                      
data WC = CountFile String | GetData String

WCType : WC -> Type                       
WCType (CountFile x) = ()
WCType (GetData x) = (Maybe WCData)

countFile : (files : List (String, WCData)) -> 
            (fname : String) ->
            Process WCType  (List (String, WCData)) Sent Sent
countFile files fname = do Right str <- Action (readFile fname)
                                     | Left err => Pure files
                           Action (putStrLn ("Count complete for " ++ fname))
                           Pure ((fname, doCount str) :: files)
                           
wcService : (loaded : List (String, WCData)) -> Service WCType ()
wcService loaded
  = do msg <- Respond (\msg => case msg of (CountFile fname) => Pure ()
                                           (GetData fname) => Pure (lookup fname loaded))
       newfiles <- case msg of (Just (CountFile fname)) => countFile loaded fname
                               _ => Pure loaded
       Loop (wcService newfiles) 
       
procMain : Client ()
procMain = do Just pid <- Spawn (wcService [])
                       | Nothing => Action (putStrLn "spawn failed.") 
              Action (putStrLn "Counting file test.txt")
              Request pid (CountFile "test.txt")
              Action (putStrLn "Processing test.txt")
              Just res <- Request pid (GetData "test.txt")
                       | Nothing => Action (putStrLn "file error")
              Action (putStrLn ("lines: " ++ show (numLines res)))
              Action (putStrLn ("words: " ++ show (numWords res)))


