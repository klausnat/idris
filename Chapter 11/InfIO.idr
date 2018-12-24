data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO
  
data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

loopPrint : String -> InfIO  
loopPrint msg = Do (putStrLn msg)
                   (\_ => loopPrint msg )

run : Fuel -> InfIO -> IO ()
run Dry p = putStrLn "Run out of fuel" 
run (More x) (Do action cont) = do res <- action
                                   run x (cont res)
                                   
                               
{-                                       
-- Internal definition of Inf and Lazy
data DelayReason = Infinite | LazyValue

data Delayed : DelayReason -> Type -> Type where
     Delay : (val : ty) -> Delayed reason ty

Inf : Type -> Type
Inf ty = Delayed Infinite ty

Lazy : Type -> Type
Lazy ty = Delayed LazyValue ty

Force : Delayed reason ty -> ty
-}
