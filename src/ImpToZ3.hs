{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module ImpToZ3 where

import           Control.Monad (foldM, (=<<))

import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust)

import           Z3.Monad      (AST, Z3)
import qualified Z3.Monad      as Z3

import           Imp

type Z3Var = AST

type Vars = Map Name Z3Var

getZ3Var :: Name -> Vars -> Z3Var
getZ3Var name scope = fromJust $ Map.lookup name scope

width :: Int
width = 32

bound :: Int
bound = 1000

unroll :: Int -> Cmd -> Cmd
unroll bound = \case
  While cond body -> unrollLoop bound (While cond body)
  c_1 `Seq` c_2   -> unroll bound c_1 `Seq` unroll bound c_2
  If cond c_1 c_2 -> If cond (unroll bound c_1) (unroll bound c_2)
  cmd             -> cmd
  where unrollLoop 0 _                      = Skip
        unrollLoop n loop@(While cond body) = If cond (body `Seq` loop) Skip


op :: (AST -> AST -> Z3 AST) -> Z3 AST -> Z3 AST -> Z3 AST
op f a b = do a' <- a; b' <- b; f a' b'

aexp :: Vars -> AExp -> Z3 AST
aexp scope = \case
  Lit n       -> Z3.mkBvNum width n
  Var name    -> return . fromJust $ Map.lookup name scope
  e_1 :+: e_2 -> op Z3.mkBvadd (aexp scope e_1) (aexp scope e_2)
  e_1 :-: e_2 -> op Z3.mkBvsub (aexp scope e_1) (aexp scope e_2)
  e_1 :*: e_2 -> op Z3.mkBvmul (aexp scope e_1) (aexp scope e_2)
  e_1 :/: e_2 -> op Z3.mkBvsdiv (aexp scope e_1) (aexp scope e_2)


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

                  -- TODO: Idea for bounds: create a *list* of
                  -- assertions and then only take up to n
                  -- steps. Shows off laziness! Probably won't work
                  -- with the Z3 monad though :/.

cmd :: Vars -> Cmd -> Z3 Vars
cmd scope = compile . unroll bound
  where compile = \case
          Skip            -> return scope
          Set name val    -> do newVal <- aexp scope val
                                newVar <- Z3.mkFreshBvVar (show name) width
                                constraint <- Z3.mkEq newVar newVal
                                Z3.assert constraint
                                return $ Map.insert name newVar scope
          Seq c_1 c_2     -> do scope'  <- cmd scope c_1
                                scope'' <- cmd scope' c_2
                                return scope''
          If cond c_1 c_2 -> do cond'   <- bexp scope cond
                                scope'  <- cmd scope c_1
                                scope'' <- cmd scope c_2
                                makePhis cond' scope scope' scope''
          _               -> error "Loops have to be unrolled before compiling to SMT!"

-- | Encodes the result of a conditional by asserting new values for
-- each variable depending on which branch was taken. Example:
--
--  y := 10;
--  if cond { x := 1; y := y + 10 }
--     else { y := y + 11; z := y }
--
--  x_1 = 1
--  y_2 = y_1 + 10
--  y_3 = y_ 1 + 11
--  z_1 = y_3
--  x_2 = ite(cond, x_1, x_0)
--  y_4 = ite(cond,y_2, y_3)
--  z_2 = ite(cond, z_0, z_1)
makePhis :: AST -> Vars -> Vars -> Vars -> Z3 Vars
makePhis cond original scope' scope'' = foldM go original $ Map.keys original
  where go scope name = do
          newVar <- Z3.mkFreshBvVar (show name) width
          constraint <- Z3.mkIte cond (getZ3Var name scope') (getZ3Var name scope'')
          Z3.assert constraint
          return $ Map.insert name newVar scope
