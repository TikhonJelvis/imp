{-# LANGUAGE RecordWildCards #-}
module Parse where

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Expr
import           Text.Parsec.Language (javaStyle)
import           Text.Parsec.String
import qualified Text.Parsec.Token    as Token

import           Imp                  (AExp (..), BExp (..), Cmd (..), Name (..))

-- pretending Haskell has a good module system…
Token.TokenParser {..} = Token.makeTokenParser javaStyle

binary name fun = Infix (fun <$ reservedOp name) AssocLeft

name = Name <$> identifier

aexp :: Parser AExp
aexp = buildExpressionParser table term
 where term =  Lit . fromIntegral <$> integer
           <|> Var <$> name
       table = [ [ binary "+" (:+:), binary "-" (:-:) ]
               , [ binary "*" (:*:) ]
               ]

bexp :: Parser BExp
bexp = buildExpressionParser table term
  where term =  True' <$ reserved "true"
            <|> False' <$ reserved "false"
            <|> (:<=:) <$> (aexp <* reservedOp "<=") <*> aexp
            <|> (:==:) <$> (aexp <* reservedOp "==") <*> aexp
        table = [ [ Prefix (Not <$ reservedOp "!") ]
                , [ binary "&&" (:&:), binary "||" (:|:) ]
                ]

cmd :: Parser Cmd
cmd = foldl Seq Skip <$> (statement `sepBy` reservedOp ";")
  where statement =  Skip <$ reserved "skip" 
                 <|> Set <$> (name <* reservedOp ":=") <*> aexp
                 <|> If <$> (reserved "if" *> bexp)
                        <*> braces cmd
                        <*> (reserved "else" *> braces cmd)
                 <|> While <$> (reserved "while" *> bexp)
                           <*> braces cmd
