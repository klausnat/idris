import System.Concurrency.Channels

adder : IO ()
adder = do Just sender_chan <- listen 1
                | Nothing => adder
           ?sss
           
main : IO ()
main = do Just adder_id <- spawn adder
                | Nothing => putStrLn "Spawn failed"
          Just chan <- connect adder_id
                | Nothing => putStrLn "Connection failed"
          ?fff 
                
