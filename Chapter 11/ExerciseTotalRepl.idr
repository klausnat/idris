data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

run : Fuel -> InfIO -> IO ()
run Dry y = do putStrLn "No fuel"
run (More fuel) (Do y f) = do res <- y
                              run fuel (f res)

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do putStrLn prompt
                             txt <- getLine
                             totalREPL (action txt) action 
