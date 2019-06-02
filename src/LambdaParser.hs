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

signedInteger :: Parser Int
signedInteger = L.signed sc integer

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
lambdaParser = between sc eof infixExpr

expr :: Parser Expr
expr = parens infixExpr
    <|> boolLit
    <|> intLit
    <|> charLit
    <|> notExpr
    -- <|> infixExpr
    <|> ifExpr
    <|> lambdaExpr
    <|> letExpr
    <|> letRecExpr
    <|> varExpr
    <|> applyExpr
    <|> caseExpr

boolLit :: Parser Expr
boolLit = (EBoolLit True <$ rword "true")
    <|> (EBoolLit False <$ rword "false")

intLit :: Parser Expr
intLit = EIntLit <$> signedInteger

charLit :: Parser Expr
charLit = ECharLit <$> charLiteral

notExpr :: Parser Expr
notExpr = do
    rword "not"
    e <- infixExpr
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
    e1 <- infixExpr
    rword "then"
    e2 <- infixExpr
    rword "else"
    e3 <- infixExpr
    return $ EIf e1 e2 e3

lambdaExpr :: Parser Expr
lambdaExpr = do
    rword "lambda"
    t <- typeParser
    symbol "=>"
    s <- identifier
    symbol "->"
    e <- infixExpr
    return $ ELambda (s, t) e

letExpr :: Parser Expr
letExpr = do
    rword "let"
    s <- identifier
    symbol ":="
    es <- infixExpr
    rword "in"
    e <- infixExpr
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
    symbol "{"
    fe <- infixExpr
    symbol "}"
    rword "in"
    e <- infixExpr
    return $ ELetRec fn (paramn, paramt) (fe, ft) e

varExpr :: Parser Expr
varExpr = do
    s <- identifier
    return $ EVar s

applyExpr :: Parser Expr
applyExpr = do
    rword "apply"
    e1 <- infixExpr
    rword "to"
    e2 <- infixExpr
    return $ EApply e1 e2

caseExpr :: Parser Expr
caseExpr = undefined

main :: IO()
main = do
    input <- getContents
    parseTest lambdaParser input
    Right e <- return $ runParser lambdaParser "" input
    putStrLn "\nEvaluating Type:"
    t <- return $ EvalType.evalType (Program [] e)
    print t
    putStrLn "\nEvaluating Value:"
    case t of
        Just TBool -> do
            r <- return $ EvalValue.evalValue (Program [] e)
            print r
        Just TInt -> do
            r <- return $ EvalValue.evalValue (Program [] e)
            print r
        Just TChar -> do
            r <- return $ EvalValue.evalValue (Program [] e)
            print r
        _ -> putStrLn "Stop evaluating value!"

