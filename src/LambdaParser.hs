module LambdaParser (main) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import EvalValue
import EvalType

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["if", "then", "else", "true", "false", "not", "and", "or",
    "lambda", "int", "bool", "char", "arrow", "data", "let", "in",
    "function", "apply", "to"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

typeParser :: Parser Type
typeParser = parens typeParser
    <|> boolTypeParser
    <|> intTypeParser
    <|> charTypeParser
    <|> arrowTypeParser
    <|> dataTypeParser

boolTypeParser :: Parser Type
boolTypeParser = TBool <$ rword "bool"

intTypeParser :: Parser Type
intTypeParser = TInt <$ rword "int"

charTypeParser :: Parser Type
charTypeParser = TChar <$ rword "char"

arrowTypeParser :: Parser Type
arrowTypeParser = do
    rword "arrow"
    t1 <- typeParser
    t2 <- typeParser
    return $ TArrow t1 t2

dataTypeParser :: Parser Type
dataTypeParser = do
    rword "data"
    adtName <- identifier
    return $ TData adtName

lambdaParser :: Parser Expr
lambdaParser = between sc eof expr

expr :: Parser Expr
expr = parens expr
    <|> boolLit
    <|> intLit
    <|> charLit
    <|> notExpr
    <|> infixExpr
    <|> ifExpr
    <|> lambdaExpr
    <|> letExpr
    <|> letRecExpr
    <|> varExpr
    <|> applyExpr
    <|> caseExpr

boolLit :: Parser Expr
boolLit = parens boolLit
    <|> (EBoolLit True <$ rword "true")
    <|> (EBoolLit False <$ rword "false")

intLit :: Parser Expr
intLit = parens intLit
    <|> EIntLit <$> integer

charLit :: Parser Expr
charLit = parens charLit
    <|> ECharLit <$> charLiteral

notExpr :: Parser Expr
notExpr = do
    rword "not"
    e <- expr
    return $ ENot e

infixExpr :: Parser Expr
infixExpr = makeExprParser expr ops

ops :: [[Operator Parser Expr]]
ops =
    [ [InfixL (EMul <$ symbol "*"),
       InfixL (EDiv <$ symbol "/")],
      [InfixL (EAdd <$ symbol "+"),
       InfixL (ESub <$ symbol "-")],
      [InfixL (EMod <$ symbol "%")],
      [InfixL (EEq  <$ symbol "="),
       InfixL (ENeq <$ symbol "!=")],
      [InfixL (ELt  <$ symbol "<"),
       InfixL (EGt  <$ symbol ">"),
       InfixL (ELe  <$ symbol "<="),
       InfixL (EGe  <$ symbol ">=")],
      [InfixL (EAnd <$ rword "and"),
       InfixL (EOr  <$ rword "or")] ]

ifExpr :: Parser Expr
ifExpr = do
    rword "if"
    e1 <- expr
    rword "then"
    e2 <- expr
    rword "else"
    e3 <- expr
    return $ EIf e1 e2 e3

lambdaExpr :: Parser Expr
lambdaExpr = do
    rword "lambda"
    t <- typeParser
    symbol "=>"
    s <- identifier
    symbol "->"
    e <- expr
    return $ ELambda (s, t) e

letExpr :: Parser Expr
letExpr = do
    rword "let"
    s <- identifier
    symbol ":="
    es <- expr
    rword "in"
    e <- expr
    return $ ELet (s, es) e

letRecExpr :: Parser Expr
letRecExpr = do
    rword "function"
    ft <- typeParser
    fn <- identifier
    symbol "("
    paramt <- typeParser
    paramn <- identifier
    symbol ")"
    fe <- expr
    rword "in"
    e <- expr
    return $ ELetRec fn (paramn, paramt) (fe, ft) e

varExpr :: Parser Expr
varExpr = do
    s <- identifier
    return $ EVar s

applyExpr :: Parser Expr
applyExpr = do
    rword "apply"
    e1 <- expr
    rword "to"
    e2 <- expr
    return $ EApply e1 e2

caseExpr :: Parser Expr
caseExpr = undefined

main :: IO()
main = do
    input <- getContents
    parseTest lambdaParser input
    -- Right e <- return $ runParser lambdaParser "" input
    -- t <- return $ EvalType.evalType (Program [] e)
    -- case t of
    --     Just TBool -> putStrLn "TBool"
    --     Just TInt -> putStrLn "TInt"
    --     Just TChar -> putStrLn "TChar"
    --     Nothing -> putStrLn "Nothing"

