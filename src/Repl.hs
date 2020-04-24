module Repl
    ( repl
    )
where

import           System.Console.Haskeline

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        input <- getInputLine ">>> "
        case input of
            Nothing     -> return ()
            Just "quit" -> return ()
            Just input  -> do
                outputStrLn input
                loop
