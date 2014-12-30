module Compile where

import Parser
import Data.Char (ord)
import Text.Printf (printf)
import Control.Monad.State

data Buffer = Buffer {
    code :: String,
    temp :: Int
} deriving (Show)

initialBuffer = Buffer "" 288

statement :: Statement -> String
statement s =
    case s of
    Assignment var expr ->
        (printf "\tl.d    $f2, %d($s1)\n" rhs :: String)                ++
        (printf "\ts.d    $f2, %d($s1)\n\n" (varToAddr var) :: String)  ++
        asm
        where (asm, rhs) = expression expr
    Print _ -> "Output:"

factor :: Factor -> Int
factor f = case f of
    Factor expr ->
        let (_, addr) = expression expr
        in addr
    Var var -> varToAddr var
    Val val -> valToAddr val

expression :: Expression -> (String, Int)
expression exp = case exp of
    Expression t []   -> term t
    Expression t subs -> subExpressions subs

subExpression :: Int -> SubExpression -> (String, Int)
subExpression address (SubExpression op t) =
    let temp = 288 in
    let (t', _) = term t in
    ((printf "\tl.d    $f2, %d($s1)\n" address :: String)   ++
     (printf "\tl.d    $f4, %d($s1)\n" t' :: String)        ++
     fromOp op                                              ++
     (printf "\ts.d    $f6, %d($s1)\n\n" temp :: String),
     temp)

subExpressions :: [SubExpression] -> (String, Int)
subExpressions = foldl subExpressions' ("", 0)
    where subExpressions' (s, addr) se =
            let (s', addr') = subExpression addr se
            in (s ++ s', addr')

term :: Term -> (String, Int)
term t = case t of
    Term f []   -> ("", factor f)
    Term f subs -> subTerms subs

subTerm :: Int -> SubTerm -> (String, Int)
subTerm address (SubTerm op f) =
    let temp = 288 in
    ((printf "\tl.d    $f2, %d($s1)\n" address :: String)       ++
     (printf "\tl.d    $f4, %d($s1)\n" (factor f) :: String)    ++
     fromOp op                                                  ++
     (printf "\ts.d    $f6, %d($s1)\n\n" temp :: String),
     temp)

subTerms :: [SubTerm] -> (String, Int)
subTerms = foldl subTerms' ("", 0)
    where subTerms' (s, addr) st =
            let (s', addr') = subTerm addr st
            in (s ++ s', addr')

fromOp :: Op -> String
fromOp Add  = "\tadd.d  $f6, $f2, $f4\n"
fromOp Sub  = "\tsub.d  $f6, $f2, $f4\n"
fromOp Mult = "\tmul.d  $f6, $f2, $f4\n"
fromOp Div  = "\tdiv.d  $f6, $f2, $f4\n"

valToAddr :: Char -> Int
valToAddr num = (ord num - ord '0') * 8

varToAddr :: Char -> Int
varToAddr var = (ord var - ord 'a' + 10) * 8

nextTemp :: Int -> Int
nextTemp cur = cur + 8

preamble :: String
preamble =
    "# preamble\n"                       ++
    "main:   addu   $s7, $ra, $zero\n"   ++
    "        la     $s1, M\n"

postamble :: String
postamble =
    "\n# postamble\n"                                       ++
    "\taddu     $ra, $s7, $zero\n"                          ++
    "\tjr       $ra                     # ret to sys\n\n"   ++
    "\t.data\n"                                             ++
    "\t.align   3\n"                                        ++
    "M:      .double  0.,1.,2.,3.,4.,5.\n"                  ++
    "\t.double  6.,7.,8.,9.             # cons\n"           ++
    "\t.space   208                     # a to z\n"         ++
    "\t.space   1000                    # 125 temps\n"      ++
    "Blank:  .asciiz \" \"\n"                               ++
    "NewL:   .asciiz \"\\n\"\n"                             ++
    "Tab:    .asciiz \"\\t\"\n"
