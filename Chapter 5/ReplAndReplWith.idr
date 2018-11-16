module Main 

||| Implement your own version of function repl

replNatasha : (prompt: String ) -> (onInput : String -> String) -> IO ()
replNatasha prompt onInput = do putStr prompt
                                input <- getLine 
                                putStrLn (onInput input)
                                replNatasha prompt onInput

{-- main to test replNatasha - analog of repl 
main : IO ()
main = replNatasha "Write youre name: " (\x => "Hello " ++ x ++ "! \n")
--}


||| Implement your own version of function replWith

replWithNatasha : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String,a)) -> IO ()
replWithNatasha state prompt onInput = do putStr prompt
                                          input <- getLine 
                                          pure (Just (res,newState)) <- onInput state input | Nothing => replWithNatasha state prompt onInput
                                          putStrLn res
                                          replWithNatasha newState prompt onInput


sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp = let val = cast inp in
                        if val < 0 then Nothing
                        else let newVal = tot + val in 
                                 Just ("Subtotal: " ++ show newVal ++ "\n", newVal)

{-- main to test replWithNatasha, analog of replWith --}
main : IO ()
main = replWithNatasha 0 "Value: " sumInputs
