module Imp where

-- | The int parameter makes it easy to create new versions of a
-- variable.
newtype Name = Name String deriving (Eq, Ord)

instance Show Name where show (Name s) = s

at :: Name -> Int -> Name
at (Name str) step = Name $ str ++ "_" ++ show step

type Scope = [(Name, Int)]

data AExp = Lit Int
          | Var Name
          | AExp :+: AExp
          | AExp :-: AExp
          | AExp :*: AExp

data BExp = True'
          | False'
          | AExp :<=: AExp
          | AExp :==: AExp
          | BExp :|: BExp
          | BExp :&: BExp
          | Not BExp

data Cmd = Skip
         | Set Name AExp
         | Seq Cmd Cmd
         | If BExp Cmd Cmd
         | While BExp Cmd

evalAExp :: Scope -> AExp -> Maybe Int
evalAExp _ (Lit i)        = Just i
evalAExp scope (Var name) = lookup name scope
evalAExp scope (e_1 :+: e_2) = 
  (+) <$> evalAExp scope e_1 <*> evalAExp scope e_2
evalAExp scope (e_1 :-: e_2) = 
  (-) <$> evalAExp scope e_1 <*> evalAExp scope e_2
evalAExp scope (e_1 :*: e_2) = 
  (*) <$> evalAExp scope e_1 <*> evalAExp scope e_2

evalBExp :: Scope -> BExp -> Maybe Bool
evalBExp scope True'  = Just True
evalBExp scope False' = Just False
evalBExp scope (e_1 :<=: e_2) =
  (<=) <$> evalAExp scope e_1 <*> evalAExp scope e_2
evalBExp scope (e_1 :==: e_2) =
  (==) <$> evalAExp scope e_1 <*> evalAExp scope e_2
evalBExp scope (e_1 :|: e_2) =
  (||) <$> evalBExp scope e_1 <*> evalBExp scope e_2
evalBExp scope (e_1 :&: e_2) =
  (&&) <$> evalBExp scope e_1 <*> evalBExp scope e_2
evalBExp scope (Not e) = not <$> evalBExp scope e

evalCmd :: Scope -> Cmd -> Maybe Scope
evalCmd scope Skip = Just scope
evalCmd scope (Set name val) = set <$> evalAExp scope val
  where set int = (name, int) : filter (\ (var, _) -> var /= name) scope
evalCmd scope (Seq c_1 c_2) = 
  do scope' <- evalCmd scope c_1
     evalCmd scope' c_2
evalCmd scope (If cond c_1 c_2) = 
  do res <- evalBExp scope cond
     if res then evalCmd scope c_1
            else evalCmd scope c_2
evalCmd scope loop@(While cond body) =
  do res <- evalBExp scope cond
     if res then evalCmd scope (Seq body loop)
            else return scope
