import Data.Vect

data ATMState = Ready | CardInserted | Session

data HasCard : ATMState -> Type where
     HasCI : HasCard CardInserted 
     HasSession : HasCard Session  

PIN : Type
PIN = Vect 4 Char

testPIN : PIN
testPIN = ['1','2','3','4']

data PINCheck = CorrectPIN | IncorrectPIN

data ATMCmd : (ty : Type) -> ATMState -> (ty -> ATMState) -> Type where
              
              InsertCard : ATMCmd () Ready (const CardInserted)
              EjectCard : {auto prf : HasCard state} -> ATMCmd () state (const Ready)
              
              GetPIN : ATMCmd PIN CardInserted (const CardInserted)
              CheckPIN : PIN -> ATMCmd PINCheck CardInserted 
                                                (\check => case check of
                                                                CorrectPIN => Session
                                                                IncorrectPIN => CardInserted)
              
              GetAmount : ATMCmd Nat state (const state) 
              Dispense : (amount : Nat) -> ATMCmd () Session  (const Session)
              
              Message : String -> ATMCmd () state (const state)
              
              Pure : (res : ty) -> ATMCmd ty (state_fn res) state_fn
              (>>=) : ATMCmd a state1 state2_fn ->
                      ((res : a) -> ATMCmd b (state2_fn res) state3_fn) ->
                      ATMCmd b state1 state3_fn
                      
                      
atm : ATMCmd () Ready (const Ready)
atm = do InsertCard
         pin <- GetPIN
         pinOK <- CheckPIN pin
         case pinOK of CorrectPIN => do cash <- GetAmount
                                        Dispense cash
                                        Message "Please remove your card and cash"
                                        EjectCard
                       IncorrectPIN => do Message "Incorrect Pin"
                                          EjectCard 

readVect : (n : Nat) -> IO (Vect n Char)
readVect Z = pure []
readVect (S k) = do c <- getChar
                    chars <- readVect k
                    pure (c :: chars)

runATM : ATMCmd res inState outState_fn -> IO res
runATM InsertCard = do putStrLn "Insert card. (Press enter)"
                       x <- getLine
                       pure ()
runATM EjectCard = putStrLn "Card ejected"
runATM GetPIN = do putStrLn "Enter pin"
                   readVect 4
runATM (CheckPIN xs) = do if xs == testPIN then pure CorrectPIN else pure IncorrectPIN
runATM GetAmount = do putStrLn "Enter Amount"
                      res <- getLine
                      pure (cast res)
runATM (Dispense amount) = putStrLn ("Here is" ++ show amount)
runATM (Message x) = putStrLn x
runATM (Pure res) = pure res
runATM (x >>= f) = do x' <- runATM x
                      runATM (f x')
