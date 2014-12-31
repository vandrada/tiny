module Compile where

import Parser
import Data.Char (ord)
import Text.Printf (printf)

data Compiler = Compiler {
    code    :: String,  -- the compiled code
    address :: Int,     -- an address returned by factor, expression, or term
    temp    :: Int      -- the current temp address
} deriving (Show)

-- | the default compiler
compiler :: Compiler
compiler = Compiler { code = "", address = 0, temp = 288 }

statement :: Compiler -> Statement -> Compiler
statement comp s =
    case s of
    Assignment var expr ->
        let comp' = expression comp expr in
        comp' { code = code comp'                                    ++
               printf "\n# M[%d] = M[%d]\n"
                   (varToAddr var `div` 8) (address comp' `div` 8)   ++
               printf "\tl.d    $f2, %d($s1)\n" (address comp')      ++
               printf "\ts.d    $f2, %d($s1)\n" (varToAddr var)
        }
    Print (Expr expr) ->
        let out' = expression comp expr in
        out' { code = code out'                                      ++
               printf "\n# print value\n"                            ++
               printf "\tli     $v0,  3\n"                           ++
               printf "\tl.d    $f12, %d($s1)\n" (address out')      ++
               printf "\tsyscall\n"
        }
    Print (Special sp) ->
        comp { code = code comp                                      ++
              "\n# Print special character\n"                        ++
              "\tli     $v0, 4\n"                                    ++
              fromSpecial sp                                         ++
              "\tsyscall\n"
        }
    -- Get _ _ ->

factor :: Compiler -> Factor -> Compiler
factor out f = case f of
    Factor expr -> expression out expr
    Var var -> out { address = varToAddr var }
    Val val -> out { address = valToAddr val }

expression :: Compiler -> Expression -> Compiler
expression out expr = case expr of
    Expression t [] -> term out t
    Expression t subs -> let out' = term out t
                         in foldl subExpression out' subs

subExpression :: Compiler -> SubExpression -> Compiler
subExpression out (SubExpression op t) =
    let out' = term out t in
    updateCompiler out out' op

term :: Compiler -> Term -> Compiler
term out t = case t of
    Term f []   -> factor out f
    Term f subs -> let out' = factor out f
                   in foldl subTerm out' subs

subTerm :: Compiler -> SubTerm -> Compiler
subTerm out (SubTerm op f) =
    let out' = factor out f in
    updateCompiler out out' op

updateCompiler :: Compiler -> Compiler -> Op -> Compiler
updateCompiler old new op =
    new { code = code old                                   ++
          printf "\n# M[%d] = M[%d] %c M[%d]\n"
            (temp old `div` 8) (address old `div` 8)
            (opToChar op) (address new `div` 8)             ++
          printf "\tl.d    $f2, %d($s1)\n" (address old)    ++
          printf "\tl.d    $f4, %d($s1)\n" (address new)    ++
          fromOp op                                         ++
          printf "\ts.d    $f6, %d($s1)\n" (temp new)
          , temp = nextTemp $ temp old
          , address = temp new
        }

-- | Converts an Op to an instruction
fromOp :: Op -> String
fromOp Add  = "\tadd.d  $f6, $f2, $f4\n"
fromOp Sub  = "\tsub.d  $f6, $f2, $f4\n"
fromOp Mult = "\tmul.d  $f6, $f2, $f4\n"
fromOp Div  = "\tdiv.d  $f6, $f2, $f4\n"

-- | Converts an Op to a char
opToChar :: Op -> Char
opToChar Add  = '+'
opToChar Sub  = '-'
opToChar Mult = '*'
opToChar Div  = '/'

-- | Converts a SpecialChar to an instruction
fromSpecial :: SpecialChar -> String
fromSpecial N = "\tla     $a0, NewL\n"
fromSpecial B = "\tla     $a0, Blank\n"
fromSpecial T = "\tla     $a0, Tab\n"

valToAddr :: Char -> Int
valToAddr num = (ord num - ord '0') * 8

varToAddr :: Char -> Int
varToAddr var = (ord var - ord 'a' + 10) * 8

nextTemp :: Int -> Int
nextTemp cur = cur + 8

preamble :: String
preamble =
    "# preamble\n"                              ++
    "# code compiled from a Tiny(r) program"    ++
    "main:   addu   $s7, $ra, $zero\n"          ++
    "        la     $s1, M"

postamble :: String
postamble =
    "# postamble\n"                                         ++
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
