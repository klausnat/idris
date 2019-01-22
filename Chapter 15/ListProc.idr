module ProcessLib

import System.Concurrency.Channels

%default total

export
data MessagePID : (iface : reqType -> Type) -> Type where
     MkMessage : PID -> MessagePID iface

public export
data ProcState = NoRequest | Sent | Complete

public export
data Process : (iface : reqType -> Type) -> Type -> (in_state : ProcState) -> (out_state : ProcState) -> Type where
     Action : IO a -> Process iface a st st
     Pure : a -> Process iface a st st
     (>>=) : Process iface a st1 st2 -> (a -> Process iface b st2 st3) -> Process iface b st1 st3
     Spawn : Process service_iface () NoRequest Complete -> Process iface (Maybe (MessagePID service_iface)) st st
     Request : MessagePID service_iface -> (msg : service_reqType) -> Process iface (service_iface msg) st st
     Respond : ((msg : reqType) -> Process iface (iface msg) NoRequest NoRequest) -> Process iface (Maybe reqType) st Sent
     Loop : Inf (Process iface a NoRequest Complete) -> Process iface a Sent Complete

public export 
data Fuel = Dry | More (Lazy Fuel)

export partial
forever : Fuel
forever = More forever

public export
Service : (iface : reqType -> Type) -> Type -> Type 
Service iface a = Process iface a NoRequest Complete

public export
NoRecv : Void -> Type
NoRecv= const Void

public export
Client : Type -> Type
Client a = Process NoRecv a NoRequest NoRequest

export
run : Fuel -> Process iface t in_state out_state -> IO (Maybe t)     
run Dry _ = pure Nothing
run fuel (Action act) = do res <- act
                           pure (Just res)
run fuel (Pure x) = pure (Just x)
run fuel (act >>= next) = do (Just x) <- run fuel act 
                                        | Nothing => pure Nothing
                             run fuel (next x)
                             
run fuel (Spawn proc) = do Just pid <- spawn (do run fuel proc
                                                 pure ())
                               | Nothing => pure Nothing
                           pure (Just (Just (MkMessage pid)))
run fuel (Request {service_iface} (MkMessage process) msg) = do Just chan <- connect process
                                                                     | _ => pure Nothing
                                                                ok <- unsafeSend chan msg
                                                                if ok then do Just x <- unsafeRecv (service_iface msg) chan
                                                                                   | Nothing => pure Nothing
                                                                              pure (Just x)
                                                                else pure Nothing
run fuel (Respond {reqType} f) = do Just sender <- listen 1
                                                | Nothing => pure (Just Nothing)
                                    Just msg <- unsafeRecv reqType sender
                                         | Nothing => pure (Just Nothing)
                                    Just res <- run fuel (f msg)
                                         | Nothing => pure Nothing
                                    unsafeSend sender res
                                    pure (Just (Just msg))

run (More fuel) (Loop act) = run fuel act

export partial
runProc : Process iface () in_state out_state -> IO ()
runProc proc = do run forever proc 
                  pure ()


data ListAction : Type where
     Length : List elem -> ListAction
     Append : List elem -> List elem -> ListAction
     
ListType : ListAction -> Type
ListType (Length xs) = Nat
ListType (Append {elem} xs ys) = List elem

procList : Service ListType ()
procList = do Respond (\msg => case msg of
                                    Length xs => Pure (length xs)
                                    Append xs ys => Pure (xs ++ ys)
                      )
              Loop procList
procMain : Client ()
procMain = do Just list <- Spawn procList
                        | Nothing => Action (putStrLn "Spawn failed")
              len <- Request list (Length [1,2,3]) 
              Action (printLn len)
              
              app <- Request list (Append [1,2,3] [4,5,6])
              Action (printLn app)
