module ImpToZ3 where

import           Control.Monad (join)

import           Z3.Monad (Z3, AST)
import qualified Z3.Monad as Z3

import           Imp

width :: Int
width = 32

var :: Name -> Z3 AST
var name = Z3.mkFreshBvVar (show name) width

op :: (AST -> AST -> Z3 AST) -> Z3 AST -> Z3 AST -> Z3 AST
op f a b = do a' <- a; b' <- b; f a' b'

aexp :: AExp -> Z3 AST
aexp (Lit n)    = Z3.mkBvNum width n
aexp (Var name) = var name
aexp (e_1 :+: e_2) = op Z3.mkBvadd (aexp e_1) (aexp e_2)
aexp (e_1 :-: e_2) = op Z3.mkBvsub (aexp e_1) (aexp e_2)
aexp (e_1 :*: e_2) = op Z3.mkBvmul (aexp e_1) (aexp e_2)

bexp :: BExp -> Z3 AST
bexp True'  = Z3.mkBool True
bexp False' = Z3.mkBool False
bexp (e_1 :<=: e_2) = op Z3.mkLe (aexp e_1) (aexp e_2)
bexp (e_1 :==: e_2) = op Z3.mkEq (aexp e_1) (aexp e_2)
bexp (b_1 :|: b_2)  = do b_1 <- bexp b_1; b_2 <- bexp b_2; Z3.mkOr [b_1, b_2]
bexp (b_1 :&: b_2)  = do b_1 <- bexp b_1; b_2 <- bexp b_2; Z3.mkAnd [b_1, b_2]
bexp (Not b)        = Z3.mkNot =<< bexp b

cmd :: Int -> [Name] -> Cmd -> Z3 ()
cmd limit step = go
  where go Skip           = return ()
        go (Set name val) = 
