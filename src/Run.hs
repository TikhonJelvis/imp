module Run where

import Parse
import Imp
import qualified ImpToZ3 as Z3

run :: String -> IO ()
run input = case parseCmd "<interactive>" input of
  Left err  -> print err
  Right cmd -> print $ evalCmd [] cmd

repl :: IO ()
repl = go []
  where go scope = do putStr "> "
                      x <- getLine
                      case parseCmd "<interactive>" x of
                        Left err  -> do print err; go scope
                        Right cmd -> case evalCmd scope cmd of
                          Just scope' -> do print scope'; go scope'
                          Nothing     -> do putStrLn "Error!"; go scope
