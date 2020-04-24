module Repl
    ( repl
    )
where

import           System.Console.Haskeline

import           Parser

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
                case parse' input of
                    Left  err  -> outputStrLn $ show err
                    Right expr -> outputStrLn $ show expr
                loop
