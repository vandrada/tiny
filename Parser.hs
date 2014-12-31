-- TODO special characters in print
-- TODO fromScreen
module Parser(
    statements
    , Var
    , Val
    , Op (..)
    , Factor (..)
    , Term (..)
    , SubTerm (..)
    , Expression (..)
    , SubExpression (..)
    , Statement (..)
) where

import Text.ParserCombinators.Parsec

type Var = Char
type Val = Char

data Op = Mult | Div | Add | Sub
    deriving (Show)
data Factor = Factor Expression | Var Var | Val Val
    deriving (Show)
data Term = Term Factor [SubTerm]
    deriving (Show)
data SubTerm = SubTerm Op Factor
    deriving (Show)
data Expression = Expression Term [SubExpression]
    deriving (Show)
data SubExpression = SubExpression Op Term
    deriving (Show)
data Out = Expr Expression | Special Char
    deriving (Show)
data Statement = Assignment Var Expression | Print Out | Get Char Var
    deriving (Show)

--
-- Main parsers
--
statements :: Parser [Statement]
statements = do
    ss <- many statement
    char '$'
    return ss

statement :: Parser Statement
statement = toScreen <|> assignment

assignment :: Parser Statement
assignment = do
    var <- letter
    char '='
    val <- expression
    char ';'
    return $ Assignment var val

toScreen :: Parser Statement
toScreen = do
    char '<'
    expr <- expression
    char ';'
    return $ Print $ Expr expr

--
-- Low Parsers
--
factor :: Parser Factor
factor = parenFactor <|> rawVar <|> rawVal

expression :: Parser Expression
expression = do
    t1 <- term
    s <- try $ many subExpression
    return $ Expression t1 s

subExpression :: Parser SubExpression
subExpression = do
    op <- expressionOps
    t <- term
    return $ SubExpression (toOp op) t

term :: Parser Term
term = do
    f <- factor
    sub <- try $ many subTerm
    return $ Term f sub

subTerm :: Parser SubTerm
subTerm = do
    op <- termOps
    t <- factor
    return $ SubTerm (toOp op) t

parenFactor :: Parser Factor
parenFactor = do
    openParen
    expr <- expression
    closeParen
    return $ Factor expr

rawVar :: Parser Factor
rawVar = do
    var <- letter
    return $ Var var

rawVal :: Parser Factor
rawVal = do
    val <- digit
    return $ Val val

openParen :: Parser Char
openParen = char '('

closeParen :: Parser Char
closeParen = char ')'

termOps :: Parser Char
termOps = oneOf "*/"

expressionOps :: Parser Char
expressionOps = oneOf "+-"

toOp :: Char -> Op
toOp op = case op of
    '+' -> Add
    '-' -> Sub
    '*' -> Mult
    '/' -> Div
