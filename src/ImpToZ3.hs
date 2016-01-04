{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module ImpToZ3 where

import           Control.Monad (join)

import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust)

import           Z3.Monad      (AST, Z3)
import qualified Z3.Monad      as Z3

import           Imp

data Z3Var = Z3Var { step :: Int, ast :: AST }

type Vars = Map Name Z3Var

getZ3Var :: Name -> Vars -> Z3Var
getZ3Var name scope = fromJust $ Map.lookup name scope

width :: Int
width = 32

op :: (AST -> AST -> Z3 AST) -> Z3 AST -> Z3 AST -> Z3 AST
op f a b = do a' <- a; b' <- b; f a' b'

aexp :: Vars -> AExp -> Z3 AST
aexp scope = \case
  Lit n       -> Z3.mkBvNum width n
  Var name    -> return . ast . fromJust $ Map.lookup name scope
  e_1 :+: e_2 -> op Z3.mkBvadd (aexp scope e_1) (aexp scope e_2)
  e_1 :-: e_2 -> op Z3.mkBvsub (aexp scope e_1) (aexp scope e_2)
  e_1 :*: e_2 -> op Z3.mkBvmul (aexp scope e_1) (aexp scope e_2)


bexp :: Vars -> BExp -> Z3 AST
bexp scope = \case
  True'        -> Z3.mkBool True
  False'       -> Z3.mkBool False
  e_1 :<=: e_2 -> op Z3.mkLe (aexp scope e_1) (aexp scope e_2)
  e_1 :==: e_2 -> op Z3.mkEq (aexp scope e_1) (aexp scope e_2)
  b_1 :|: b_2  -> do b_1 <- bexp scope b_1
                     b_2 <- bexp scope b_2
                     Z3.mkOr [b_1, b_2]
  b_1 :&: b_2  -> do b_1 <- bexp scope b_1
                     b_2 <- bexp scope b_2
                     Z3.mkAnd [b_1, b_2]
  Not b        -> Z3.mkNot =<< bexp scope b


cmd :: Vars -> Cmd -> Z3 Vars
cmd scope = \case
  Skip -> return scope
  Set name val -> do newVal <- aexp scope val
                     let Z3Var { step, ast } = getZ3Var name scope
                     newVar <- Z3.mkFreshBvVar (show $ name `at` succ step) width
                     constraint <- Z3.mkEq newVar newVal
                     Z3.assert constraint
                     return $ Map.insert name (Z3Var (succ step) newVar) scope
  Seq c_1 c_2 -> do scope' <- cmd scope c_1
                    cmd scope c_2
