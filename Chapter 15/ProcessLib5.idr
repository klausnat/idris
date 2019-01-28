module ProcessLib5

import System.Concurrency.Channels

public export
data MessagePID : (iface : reqType -> Type) -> Type where
     MkMessage : PID -> MessagePID iface

public export
data ProcSt = NoRequest | Sent | Complete

public export
data Process : (iface : reqType -> Type) -> Type -> (in_st : ProcSt) -> (out_st : ProcSt) -> Type where
     Action : IO a -> Process iface a st st
     (>>=) : Process iface a st1 st2 -> (a -> Process iface b st2 st3) -> Process iface b st1 st3
     Pure : a -> Process iface a st st
     Spawn : Process service_iface () NoRequest Complete -> Process iface (Maybe (MessagePID service_iface)) st st
     Request : MessagePID service_iface -> (msg : reqType) -> Process iface (service_iface msg) st st
     Respond : ((msg : reqType) -> Process iface (iface msg) NoRequest NoRequest) -> Process iface (Maybe reqType) st Sent 
     Loop : Inf (Process iface a NoRequest Complete) -> Process iface a Sent Complete

public export          
NoReq : Type -> Type
NoReq = const Void     

public export
Service : (iface : reqType -> Type) -> Type -> Type
Service iface a = Process iface a NoRequest Complete

public export
Client : Type -> Type
Client a = Process NoReq a NoRequest NoRequest

public export
data Fuel = Dry | More (Lazy Fuel)

export partial
forever : Fuel
forever = More forever

export partial
run : Fuel -> Process iface a in_st out_st -> IO (Maybe a)
run Dry _ = pure Nothing
run (More fuel) (Action x) = do res <- x
                                pure (Just res)
run (More fuel) (process >>= next) = do (Just res) <- run fuel process
                                                   | Nothing => pure Nothing
                                        run fuel (next res)
run (More fuel) (Pure x) = pure (Just x)
run (More fuel) (Spawn x) = do Just res <- run fuel x
                                        | Nothing => pure Nothing
                               Just pid <- spawn (pure res)   
                                        | Nothing => pure Nothing
                               pure (Just (Just (MkMessage pid)))
run (More fuel) (Request (MkMessage pid) msg {service_iface}) = do Just chan <- connect pid
                                                                               | Nothing => pure Nothing
                                                                   ok <- unsafeSend chan msg
                                                                   if ok then do (Just res) <- unsafeRecv (service_iface msg) chan
                                                                                            | Nothing => pure Nothing
                                                                                 pure (Just res)
                                                                   else pure Nothing   

run (More fuel) (Respond f {reqType}) = do Just chan <- listen 1
                                                     | Nothing => pure (Just Nothing)
                                           Just msg <- unsafeRecv reqType chan
                                                    | Nothing => pure (Just Nothing)
                                           Just res <- run fuel (f msg)
                                               | Nothing => pure Nothing
                                           pure (Just (Just msg))

run (More fuel) (Loop x) = run fuel x

export partial
mainProc : Process iface a st1 st2 -> IO ()
mainProc pr = do run forever pr
                 pure ()
