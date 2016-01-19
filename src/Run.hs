module Run where

import           Control.Monad ((<=<))

import qualified Data.Map as Map
import           Data.Map (Map)

import qualified Z3.Monad as Z3

import           Imp
import qualified ImpToZ3  as Z3
import           Parse

run :: String -> IO ()
run input = case parseCmd "<interactive>" input of
  Left err  -> print err
  Right cmd -> print $ evalCmd [] cmd

z3File :: FilePath -> IO ()
z3File path = do program <- readFile path
                 case parseCmd path program of
                   Left err  -> print err
                   Right cmd -> putStrLn <=< runZ3 $ do
                     Z3.forwards (Map.fromList []) cmd
                     (_, Just model) <- Z3.solverCheckAndGetModel
                     Z3.modelToString model
  where runZ3 = Z3.evalZ3With Nothing Z3.opts

repl :: IO ()
repl = go []
  where go scope = do putStr "> "
                      x <- getLine
                      case parseCmd "<interactive>" x of
                        Left err  -> do print err; go scope
                        Right cmd -> case evalCmd scope cmd of
                          Just scope' -> do print scope'; go scope'
                          Nothing     -> do putStrLn "Error!"; go scope
