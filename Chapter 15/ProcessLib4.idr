module ProcessLib4

import System.Concurrency.Channels

public export
data Fuel = Dry | More (Lazy Fuel)

public export
data ProcState = NoRequest | Sent | Complete

public export
data MessagePID : (iface : reqType -> Type) -> Type where
     MkMessage : PID -> MessagePID iface
     
public export
data Process : (iface : reqType -> Type) -> Type -> (in_state : ProcState) -> (out_state : ProcState) -> Type where
     Action : IO a -> Process iface a st st
     Pure : a -> Process iface a st st
     (>>=) : Process iface a st1 st2 -> (a -> Process iface b st2 st3) -> Process iface b st1 st3
     
     Spawn : Process service_iface () NoRequest Complete -> Process iface (Maybe (MessagePID service_iface)) st st
     
     Request : MessagePID service_iface -> (msg : reqType) -> Process iface (service_iface msg) st st
     
     Respond : ( (msg : reqType ) -> Process iface (iface msg) NoRequest NoRequest) -> Process iface (Maybe reqType) st Sent
     
     Loop : Inf (Process iface a NoRequest Complete) -> Process iface a Sent Complete
     
public export
NoRecv : Void -> Type
NoRecv = const Void
          
public export
Client : Type -> Type
Client a = Process NoRecv a NoRequest NoRequest

public export
Service : (iface : reqType -> Type) -> Type -> Type
Service iface a = Process iface a NoRequest Complete

export partial
forever : Fuel
forever = More forever

export 
run : Fuel -> Process iface t st_in st_out -> IO (Maybe t)
run Dry _ = pure Nothing
run (More x) (Action y) = do res <- y
                             pure (Just res)
run (More x) (Pure y) = pure (Just y)
run (More fuel) (proc >>= next) = do Just res <- run fuel proc
                                              | Nothing => pure Nothing
                                     run fuel (next res)

run (More fuel) (Spawn proc) = do Just res <- run fuel proc
                                           | Nothing => pure Nothing   
                                  Just pid <- spawn (pure res)
                                           | Nothing => pure Nothing
                                  pure (Just (Just (MkMessage pid)))

run (More x) (Request (MkMessage pid) msg {service_iface}) = do Just chan <- connect pid
                                                                           | Nothing => pure Nothing
                                                                ok <- unsafeSend chan msg
                                                                if ok then do Just res <- unsafeRecv (service_iface msg) chan 
                                                                                       | Nothing => pure Nothing
                                                                              pure (Just res)
                                                                else pure Nothing

run (More fuel) (Respond f {reqType}) = do Just chan <- listen 1
                                                     | Nothing => pure Nothing
                                           Just msg <- unsafeRecv reqType chan
                                                    | Nothing => pure Nothing
                                           Just res <- run fuel (f msg)
                                                    | Nothing => pure Nothing
                                           unsafeSend chan res
                                           pure (Just (Just msg))

run (More fuel) (Loop proc) = run fuel proc

export partial
runProc : Process iface () in_st out_st -> IO ()
runProc pr = do run forever pr
                pure ()
