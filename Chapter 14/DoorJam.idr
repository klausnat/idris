data DoorState = DoorOpen | DoorClosed
data DoorResult = OK | Jammed

data DoorCmd : (ty : Type) -> DoorState -> (doorState_fn : ty -> DoorState) -> Type where
     Open : DoorCmd DoorResult DoorClosed (\res => case res of 
                                                                OK => DoorOpen
                                                                Jammed => DoorClosed)
     Close : DoorCmd () DoorOpen (const DoorClosed)
     RingBell : DoorCmd () DoorClosed (const DoorClosed)
      
     Display : String -> DoorCmd () state (const state)
     
     Pure : (res : ty) -> DoorCmd ty (doorState_fn res) doorState_fn
     (>>=) : DoorCmd a state1 state2_fn ->
             ((res : a) -> DoorCmd b (state2_fn res) state3_fn) ->
             DoorCmd b state1 state3_fn

{-- full variant
doorProg : DoorCmd () DoorClosed (const DoorClosed)
doorProg = do RingBell
              jam <- Open
              Display "Trying to open the door"
              case jam of
                   OK => do Display "Glad to be of service"
                            Close
                   Jammed => Display "Door Jammed"

-}

-- concise variant
doorProg : DoorCmd () DoorClosed (const DoorClosed)
doorProg = do RingBell
              OK <- Open | Jammed => Display "Door Jammed"
              Display "Glad to be of service"
              Close

logOpen : DoorCmd DoorResult DoorClosed (\res => case res of OK => DoorOpen
                                                             Jammed => DoorClosed)
                                                             
logOpen = do Display "Trying to open the door"
             OK <- Open | Jammed => do Display "Door Jammed"
                                       Pure Jammed
             Display "Door Opened!"
             Pure OK
