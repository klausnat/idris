VendState : Type
VendState = (Nat, Nat)

data Input = COIN | VEND | CHANGE | REFILL Nat     
data CoinResult = Inserted | Rejected

data MachineCmd : (ty : Type) -> VendState -> (ty -> VendState) -> Type where
     InsertCoin : MachineCmd CoinResult (pounds, chocs) (\res => case res of 
                                                                      Inserted => (S pounds, chocs)
                                                                      Rejected => (pounds, chocs))
                                                        
     Vend       : MachineCmd () (S pounds, S chocs) (const (pounds, chocs))
     GetCoins   : MachineCmd () (pounds, chocs) (const (Z, chocs))
     Refill     : (bars : Nat) -> MachineCmd () (Z, chocs) (const (Z, bars + chocs))
     Display    : String -> MachineCmd () state (const state)
     GetInput   : MachineCmd (Maybe Input) state (const state)
     Pure       : (res : ty) -> MachineCmd ty (state_fn res) state_fn
     (>>=)      : MachineCmd a state1 state2_fn -> 
                  ((res : a) -> MachineCmd b (state2_fn res) state3_fn) -> 
                  MachineCmd b state1 state3_fn
     
data MachineIO : VendState -> Type where
     Do : MachineCmd a state1 state2_fn -> ((res : a) -> Inf (MachineIO (state2_fn res))) -> MachineIO state1
     
namespace MachineDo
  (>>=) : MachineCmd a state1 state2_fn -> ((res : a) -> Inf (MachineIO (state2_fn res))) -> MachineIO state1
  (>>=) = Do
  
mutual 
  vend : MachineIO (pounds, chocs)
  vend {pounds = S p} {chocs = S c} = do Vend
                                         Display "Enjoy!"
                                         machineLoop
  vend {pounds = Z} = do Display "Insert a coin"
                         machineLoop
  vend {chocs = Z} = do Display "Out of stock"
                        machineLoop
                                         
  
  refill : (num : Nat) -> MachineIO (pounds, chocs)
  refill num {pounds = Z} = do Refill num
                               machineLoop
  refill _ = do Display "Can't refil: Coins in machine"
                machineLoop
  
  machineLoop : MachineIO (pounds, chocs)
  machineLoop = do (Just x) <- GetInput
                        | Nothing => do Display "Invalid input"
                                        machineLoop
                   case x of COIN => do Inserted <- InsertCoin | Rejected => do Display "bad coin"
                                                                                machineLoop
                                        machineLoop
                             VEND => vend
                             CHANGE => do GetCoins
                                          Display "Change returned"
                                          machineLoop
                             REFILL num => refill num  
