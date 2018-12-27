-- Exercise 2. Extend the Command type so it also support reading and writing to files
-- Exercise 3. Implement an interactive shell that supports commands

%default total

data Input = Cat String | Copy String String | ExitCmd

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     ReadFile : String -> Command (Either FileError String)
     WriteFile : String -> String -> Command (Either FileError ())
     Pure : a -> Command a
     Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

runCommand : Command a -> IO a     
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile x) = readFile x
runCommand (WriteFile x y) = writeFile x y 
runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x
                           runCommand (f res)

namespace CommandIO
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleIO
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry y = pure Nothing
run f (Quit y) = pure (Just y)
run (More x) (Do z f) = do res <- runCommand z
                           run x (f res)

parseInput : (prompt : String) -> Command Input
parseInput prompt = do PutStr prompt
                       res <- GetLine
                       case toLower res of
                            "exit" => Pure ExitCmd
                            

terminal : ConsoleIO String
terminal = do res <- parseInput "Command: "
              case res of ExitCmd => ?ss
                          Copy source destination => ?oo
                          Cat filename => ?uu

partial
main : IO ()
main = do putStr "Bye"
          
        
