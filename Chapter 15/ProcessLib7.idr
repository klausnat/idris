module ProcessLib7
import System.Concurrency.Channels

public export
data MessagePID : (iface : reqType -> Type) -> Type where
     MkMessage : PID -> MessagePID iface

public export
data ProcState = NoRequest | Complete | Sent

public export
data Fuel = Dry | More (Lazy Fuel)

public export
data Process : (iface : reqType -> Type) -> Type -> ProcState -> ProcState -> Type where
     Action : IO a -> Process iface a st st
     Pure : a -> Process iface a st st
     (>>=) : Process iface a st1 st2 ->
             (a -> Process iface b st2 st3) ->
             Process iface b st1 st3
     Spawn : Process service_iface () NoRequest Complete -> Process iface (Maybe (MessagePID service_iface)) st st
     Request : MessagePID service_iface -> (msg : reqType) -> Process iface (Maybe (service_iface msg)) st st
     Respond : ((msg : reqType) -> Process iface (iface msg) NoRequest NoRequest) -> 
               Process iface (Maybe reqType) st Sent
     Loop : Inf (Process iface a NoRequest Complete) -> Process iface a Sent Complete

public export
NoRec : Type -> Type
NoRec = const Void

public export
Service : (iface : reqType -> Type) -> Type -> Type
Service iface a = Process iface a NoRequest Complete

public export
Client : Type -> Type
Client a = Process NoRec a NoRequest NoRequest

export partial
forever : Fuel
forever = More forever

run : Fuel -> Process iface a st1 st2 -> IO (Maybe a)
run Dry y = pure Nothing
run (More fuel) (Action x) = do res <- x
                                pure (Just res)
run (More fuel) (Pure x) = pure (Just x)
run (More fuel) (x >>= f) = do Just res <- run fuel x
                                        | Nothing => pure Nothing
                               run fuel (f res)   
run (More fuel) (Spawn x) = do Just pid <- spawn (do Just res <- run fuel x
                                                     pure res)
                               pure (Just (Just (MkMessage pid)))
run (More fuel) (Request (MkMessage pid) {service_iface} msg) = do Just chan <- connect pid
                                                                             | Nothing => pure Nothing
                                                                   ok <- unsafeSend chan msg
                                                                   if ok then do Just res <- unsafeRecv (service_iface msg) chan
                                                                                          | Nothing => pure Nothing
                                                                                 pure (Just (Just res))
                                                                   else pure Nothing           
run (More fuel) (Respond f {reqType}) = do Just chan <- listen 1
                                                     | Nothing => pure Nothing
                                           Just msg <- unsafeRecv reqType chan 
                                                    | Nothing => pure Nothing
                                           res <- run fuel (f msg)
                                               | Nothing => pure Nothing
                                           pure (Just (Just msg))
                                           
run (More fuel) (Loop x) = run fuel x

export partial
runProc : Process iface a st1 st2 -> IO ()
runProc pr = do run forever pr 
                pure ()
