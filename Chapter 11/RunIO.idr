data RunIO : Type -> Type where
     Quit : a -> RunIO a
     Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b
     
(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

greet : RunIO ()
greet = do putStrLn "Enter you name: "
           name <- getLine
           if name == "" then do putStrLn "Bye Bye!" 
                                 Quit () 
                         else do putStrLn ("Hi " ++ name ++ "! ")
                                 greet
