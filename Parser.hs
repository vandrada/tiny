module Parser(
    statements
    , Var
    , Val
    , Op (..)
    , Factor (..)
    , SpecialChar (..)
    , Out (..)
    , Term (..)
    , If (..)
    , While (..)
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

data SpecialChar = N | B | T
    deriving (Show)

data Out = Expr Expression | Special SpecialChar
    deriving (Show)

data If = IfThen Expression [Statement]
         | IfThenElse Expression [Statement] [Statement]
    deriving (Show)

data While = While Expression [Statement]
    deriving (Show)

data Statement = Assignment Var Expression | Print Out | Get Var | Cond If
                | Loop While | Terminate
    deriving (Show)

--
-- Main parsers
--
statements :: Parser [Statement]
statements = many statement

statement :: Parser Statement
statement = toScreen <|> fromScreen <|> assignment
            <|> terminate <|> cond <|> loop

assignment :: Parser Statement
assignment = do
    var <- letter
    char '='
    val <- expression
    char ';'
    return $ Assignment var val

terminate :: Parser Statement
terminate = do
    char '$'
    return Terminate

toScreen :: Parser Statement
toScreen = try (do char '<'
                   sp <- special
                   char ';'
                   return $ Print $ Special $ toSpecial sp)
       <|> try (do char '<'
                   expr <- expression
                   char ';'
                   return $ Print $ Expr expr)

fromScreen :: Parser Statement
fromScreen = do
    char '>'
    var <- letter
    char ';'
    return $ Get var

cond :: Parser Statement
cond = do
    ifThen <- try ifThen <|> try ifThenElse
    return $ Cond ifThen

loop :: Parser Statement
loop = do
    w <- while
    return $ Loop w

--
-- Low Parsers
--
while :: Parser While
while = do
    char '{'
    expr <- bool
    ss <- statements
    char '}'
    return $ While expr ss

ifThen :: Parser If
ifThen = do
    char '['
    expr <- bool
    ss <- statements
    char ']'
    return $ IfThen expr ss

ifThenElse :: Parser If
ifThenElse = do
    char '['
    expr <- bool
    ss <- statements
    char ':'
    ss' <- statements
    char ']'
    return $ IfThenElse expr ss ss'

bool :: Parser Expression
bool = do
    expr <- expression
    char '?'
    return expr

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

special :: Parser Char
special = char 'N' <|> char 'B' <|> char 'T'

openParen :: Parser Char
openParen = char '('

closeParen :: Parser Char
closeParen = char ')'

termOps :: Parser Char
termOps = oneOf "*/"

expressionOps :: Parser Char
expressionOps = oneOf "+-"

toOp :: Char -> Op
toOp '+' = Add
toOp '-' = Sub
toOp '*' = Mult
toOp '/' = Div

toSpecial :: Char -> SpecialChar
toSpecial 'N' = N
toSpecial 'B' = B
toSpecial 'T' = T
