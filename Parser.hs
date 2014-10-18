module Parser where

import Environment
import Text.ParserCombinators.Parsec

type Statements = [Statement]

data Op = Mult | Div | Add | Sub
    deriving (Show)

data Paren = Open | Close
    deriving (Show)

data Factor = EFactor Expression | RawVar Var | RawVal String
    deriving (Show)

data Term = Term Factor Op Factor | TermS Factor
    deriving (Show)

data Expression = Expression Term Op Term | ExpressionS Term
    deriving (Show)

data Statement =
      Assignment Var Expression
    | Print Char Expression
    | Get Char Var
    deriving (Show)

--
-- Main parsers
--
parseFactor :: Parser Factor
parseFactor = parseParenFactor <|> parseRawVar <|> parseRawVal

parseTerm :: Parser Term
parseTerm = parseTermT <|> parseTermS

parseExpression :: Parser Expression
parseExpression = parseExpressionT <|> parseExpressionS

parseAssignment :: Parser Statement
parseAssignment = do
    var <- letter
    char '='
    val <- parseExpression
    return $ Assignment var val

--
-- 'Low' Parsers
--
parseExpressionS :: Parser Expression
parseExpressionS = do
    t1 <- parseTerm
    return $ ExpressionS t1

parseExpressionT :: Parser Expression
parseExpressionT = do
    t1 <- parseTerm
    op <- oneOf "+-"
    t2 <- parseTerm
    case op of
        '+' -> return $ Expression t1 Add t2
        '-' -> return $ Expression t1 Sub t2

parseTermS :: Parser Term
parseTermS = do
    f1 <- parseFactor
    return $ TermS f1

parseTermT :: Parser Term
parseTermT = do
    f1 <- parseFactor
    op <- oneOf "*/"
    f2 <- parseFactor
    case op of
        '*' -> return $ Term f1 Mult f2
        '/' -> return $ Term f2 Div f2

parseOpenParen :: Parser Paren
parseOpenParen = do
    paren <- char '('
    return Open

parseCloseParen :: Parser Paren
parseCloseParen = do
    paren <- char ')'
    return Close

parseParenFactor :: Parser Factor
parseParenFactor = do
    parseOpenParen
    expr <- parseExpression
    parseCloseParen
    return $ EFactor expr

parseRawVar :: Parser Factor
parseRawVar = do
    var <- letter
    return $ RawVar var

parseRawVal :: Parser Factor
parseRawVal = do
    val <- many digit
    return $ RawVal val

