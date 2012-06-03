{-# LANGUAGE TypeSynonymInstances #-}
--
-- IO.hs
--
-- Basic input and output of expressions.
--
-- Gregory Wright, 22 April 2011
--

module Math.Symbolic.Wheeler.IO where


import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Expr as Ex
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P


import Math.Symbolic.Wheeler.Canonicalize
--import Math.Symbolic.Wheeler.CanonicalizeDebug
import Math.Symbolic.Wheeler.Function
import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Numeric
import Math.Symbolic.Wheeler.Symbol


--
-- Read an expression from a string.
--

-- Parse an expression.  Formerly, certain transformations
-- that put subexpressions into canonical form
-- were done on the fly.  This is no longer the case.  The
-- string is converted to an unsimplified expression, and
-- you must invoke the "canonicalize" function explicitly.

readExpr :: String -> Expr
readExpr = canonicalize . runLex


wheelerDef   :: P.LanguageDef st
wheelerDef    = P.LanguageDef
               { P.commentStart   = "{-"
               , P.commentEnd     = "-}"
               , P.commentLine    = "--"
               , P.nestedComments = True
               , P.identStart     = letter <|> char '\\'
               , P.identLetter    = letter <|> char '\''
               , P.opStart        = P.opLetter emptyDef
               , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , P.reservedOpNames= []
               , P.reservedNames  = []
               , P.caseSensitive  = True
               }

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser
         (wheelerDef
          { P.reservedOpNames = ["^", "*", "/", "+", "-", "!", "**", "sqrt"]
          })


whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme  = P.lexeme lexer

symbol :: String -> Parser String
symbol  = P.symbol lexer

integer :: Parser Integer
integer  = P.integer lexer

natural :: Parser Integer
natural  = P.natural lexer

float :: Parser Double
float  = P.float lexer

parens :: Parser a -> Parser a
parens  = P.parens lexer

semi :: Parser String
semi   = P.semi lexer

identifier :: Parser String
identifier = P.identifier lexer

reserved :: String -> Parser ()
reserved  = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp  = P.reservedOp lexer

commaList :: Parser a -> Parser [ a ]
commaList  = P.commaSep lexer

expr :: Parser Expr
expr  = buildExpressionParser table factor
       <?> "expression"

table :: [[ Operator String () Identity Expr ]]
table = [[ inOp   "**" (**) AssocRight] 
        ,[ preOp  "-" negate,        preOp "+" id,           preOp "sqrt" sqrt ]
        ,[ inOp   "*" (*) AssocLeft, inOp  "/" (/) AssocLeft ]
        ,[ inOp   "+" (+) AssocLeft, inOp  "-" (-) AssocLeft ]
        ]
        where
          preOp  s f       = Ex.Prefix  (do { reservedOp s; return f } <?> "prefix operator")
          inOp   s f assoc = Ex.Infix   (do { reservedOp s; return f } <?> "infix operator")  assoc

factor :: Parser Expr
factor =  try application
      <|> parens expr 
      <|> numericConst
      <|> do { x <- identifier; return (Symbol (simpleSymbol x)) }
      <?> "simple expresion"

application :: Parser Expr
application = do { f   <- reservedFunction
                 ; whiteSpace
                 ; arg <- expr
                 ; return (Applic f arg)
                 }

-- Note that the order is important.  Function names that are
-- prefixes of the other function names must be listed later.

reservedFunction :: Parser Function
reservedFunction =  do { _ <- try $ string "asinh";  return Asinh }
                <|> do { _ <- try $ string "acosh";  return Acosh }
                <|> do { _ <- try $ string "atanh";  return Atanh }
                <|> do { _ <- try $ string "asin";   return Asin }
                <|> do { _ <- try $ string "acos";   return Acos }
                <|> do { _ <- try $ string "atan";   return Atan }
                <|> do { _ <- try $ string "sinh";   return Sinh }
                <|> do { _ <- try $ string "cosh";   return Cosh }
                <|> do { _ <- try $ string "tanh";   return Tanh }
                <|> do { _ <- try $ string "sin";    return Sin }
                <|> do { _ <- try $ string "cos";    return Cos }
                <|> do { _ <- try $ string "tan";    return Tan }
                <|> do { _ <- try $ string "abs";    return Abs }
                <|> do { _ <- try $ string "signum"; return Signum }
                <|> do { _ <- try $ string "log";    return Log }
                <|> do { _ <- try $ string "exp";    return Exp }

numericConst :: Parser Expr
numericConst = do { x <- integer; return (Const (I x)) }

runLex :: String -> Expr
runLex input = let
        result = parse ( do { whiteSpace
                            ; x <- expr
                            ; eof
                            ; return x
                            }) "" input
        in
          case result of
            Right ex  -> ex
            Left  err -> error (show err)

