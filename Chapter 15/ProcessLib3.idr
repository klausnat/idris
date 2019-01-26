module ProcessLib3

import System.Concurrency.Channels
%default total

public export
data Fuel = Dry | More (Lazy Fuel)

export
data MessagePID : (iface : reqType -> Type) -> Type where
     MkMessage : PID -> MessagePID iface

public export
data ProcState = NoRequest | Sent | Complete

export partial
forever : Fuel
forever = More forever

public export
data Process : (iface : reqType -> Type) -> Type -> ProcState -> ProcState -> Type where
     Loop : Inf (Process iface a NoRequest Complete) -> Process iface a Sent Complete
     (>>=) : Process iface a st1 st2 ->
             (a -> Process iface b st2 st3) ->
             Process iface b st1 st3
     Pure : a -> Process iface a st st
     
     Request : (MessagePID service_iface) -> (msg : reqType) ->
               Process iface (service_iface msg) st st
     
     Respond : ((msg : reqType) -> 
               Process iface (iface msg) NoRequest NoRequest) -> 
               Process iface (Maybe reqType) st Sent
     
     Spawn : Process service_iface () NoRequest Complete -> 
             Process iface (Maybe (MessagePID service_iface)) st st
     Action : IO a -> Process iface a st st

public export
NoRecv : Void -> Type
NoRecv = const Void

public export
Service : (process_iface : reqType -> Type ) -> Type -> Type
Service iface a = Process iface a NoRequest Complete

public export 
Client : Type -> Type
Client a = Process NoRecv a NoRequest NoRequest

export 
run : Fuel -> Process iface t in_st out_st -> IO (Maybe t) 
run Dry _ = pure Nothing
run (More x) (Loop y) = run x y 
run (More x) (proc >>= next) = do Just res <- run x proc
                                           | Nothing => pure Nothing
                                  run x (next res)
run (More x) (Pure y) = pure (Just y)

run (More x) (Request {service_iface} (MkMessage process) msg) 
    = do Just chan <- connect process
                   | _ => pure Nothing
         ok <- unsafeSend chan msg
         if ok then do Just x <- unsafeRecv (service_iface msg) chan
                              | Nothing => pure Nothing
                       pure (Just x)
         else pure Nothing

run (More x) (Respond {reqType} f) = do Just chan <- listen 1
                                                  | Nothing => pure (Just Nothing)
                                        Just msg <- unsafeRecv reqType chan
                                                 | Nothing => pure (Just Nothing)
                                        Just res <- run x (f msg)   
                                                 | Nothing => pure Nothing   
                                        unsafeSend chan res
                                        pure (Just (Just msg))

run (More x) (Spawn proc) = do Just pid <- spawn (do run x proc
                                                     pure ())
                                        | Nothing => pure Nothing
                               pure (Just (Just (MkMessage pid)))

run (More x) (Action y) = do res <- y
                             pure (Just res)
                             

