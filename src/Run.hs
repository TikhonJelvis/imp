{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Run where

import           Control.Monad ((<=<))

import           Data.Map      (Map)
import qualified Data.Map      as Map

import qualified Z3.Monad      as Z3

import           Imp
import qualified ImpToZ3       as Z3
import           Parse

run :: String -> IO ()
run input = case parseCmd "<interactive>" input of
  Left err  -> print err
  Right cmd -> print $ evalCmd [] cmd

runFile :: FilePath -> IO ()
runFile path = do
  program <- readFile path
  case parseCmd path program of
    Left err  -> print err
    Right cmd -> case evalCmd [] cmd of
      Nothing  -> putStrLn "Scope error?"
      Just res -> print res

z3File exec scope path = do
  program <- readFile path
  case parseCmd path program of
    Left err  -> print err
    Right cmd -> putStrLn <=< runZ3 $ do
      exec scope cmd
      Z3.solverCheckAndGetModel >>= \case
        (Z3.Sat, Just model) -> Z3.modelToString model
        (result, _)          -> return $ "Failed: " ++ show result
  where runZ3 = Z3.evalZ3With (Just Z3.QF_BV) Z3.opts

gcdPath = "/home/tikhon/Documents/programming/haskell/imp/gcd.imp"

forwardsExample = z3File Z3.forwards (Map.fromList []) gcdPath

backwardsExample = z3File Z3.backwards constraints gcdPath
  where constraints = (Map.fromList [("a", 135), ("b", 135), ("d", 0)])

repl :: IO ()
repl = go []
  where go scope = do putStr "> "
                      x <- getLine
                      case parseCmd "<interactive>" x of
                        Left err  -> do print err; go scope
                        Right cmd -> case evalCmd scope cmd of
                          Just scope' -> do print scope'; go scope'
                          Nothing     -> do putStrLn "Error!"; go scope
