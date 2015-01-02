module Compile where

import Data.Char   (ord)
import Parser
import Text.Printf (printf)

data Compiler = Compiler
    { compiled :: String
    , address  :: Int
    , temp     :: Int
    , label    :: Int
    } deriving (Show)

-- | Compiles multiple statements with the default compiler
compile :: [Statement] -> Compiler
compile = foldl compileStatement Compiler { compiled = preamble, address = 0
                                            , temp = 288, label = 0}

-- | Compiles a single statement
compileStatement :: Compiler -> Statement -> Compiler
compileStatement comp s = case s of
    Assignment var expr ->
        let comp' = expression comp expr in
        comp' { compiled = compiled comp' ++ unlines [
                  printf "# M[%d] = M[%d]"
                   (varToAddr var `div` 8) (address comp' `div` 8)
                , printf "\tl.d    $f2, %d($s1)" (address comp')
                , printf "\ts.d    $f2, %d($s1)\n" (varToAddr var)]
        }
    Print (Expr expr) ->
        let comp' = expression comp expr in
        comp' { compiled = compiled comp' ++ unlines [
                  printf "# print value"
                , printf "\tli     $v0,  3"
                , printf "\tl.d    $f12, %d($s1)" (address comp')
                , printf "\tsyscall\n"]
        }
    Print (Special sp) ->
        comp { compiled = compiled comp ++ unlines [
                 "# print special character"
               , "\tli     $v0, 4"
               , fromSpecial sp
               , "\tsyscall\n"]
        }
    Get var ->
        comp { compiled = compiled comp ++ unlines [
                 printf "# read M[%d] as double" (varToAddr var `div` 8)
               , printf "\tli     $v0, 7"
               , printf "\tsyscall"
               , printf "\ts.d    $f0, %d($s1)\n" (varToAddr var)]
        }
    Cond (IfThen expr ss) ->
        ifEnd
        . jump
        . (\c -> foldl compileStatement c ss)
        . test "Else" $ expression (start "If" comp) expr
    Cond (IfThenElse expr ss ss') ->
        ifEnd
        . (\c -> foldl compileStatement c ss')
        . jump
        . (\c -> foldl compileStatement c ss)
        . test "Else" $ expression (start "If" comp) expr
    Loop (While expr ss) ->
        whileEnd
        . (\c -> foldl compileStatement c ss)
        . test "WhileEnd" $ expression (start "While" comp) expr
    Terminate -> comp { compiled = compiled comp ++ postamble }

-- | Handles a Factor
factor :: Compiler -> Factor -> Compiler
factor comp f = case f of
    Factor expr -> expression comp expr
    Var var -> comp { address = varToAddr var }
    Val val -> comp { address = valToAddr val }

-- | Handles an Expression
expression :: Compiler -> Expression -> Compiler
expression comp expr = case expr of
    Expression t [] -> term comp t
    Expression t subs -> let out' = term comp t
                         in foldl subExpression out' subs

-- | Handles a SubExpression
subExpression :: Compiler -> SubExpression -> Compiler
subExpression comp (SubExpression op t) =
    let comp' = term comp t in
    updateCompiler comp comp' op

-- | Handles a Term
term :: Compiler -> Term -> Compiler
term comp t = case t of
    Term f []   -> factor comp f
    Term f subs -> let comp' = factor comp f
                   in foldl subTerm comp' subs

-- | Handles a SubTerm
subTerm :: Compiler -> SubTerm -> Compiler
subTerm comp (SubTerm op f) =
    let comp' = factor comp f in
    updateCompiler comp comp' op

-- | Inserts code for Expression and Factors
updateCompiler :: Compiler -> Compiler -> Op -> Compiler
updateCompiler old new op =
    new { compiled = compiled old ++
          unlines [
            printf "# M[%d] = M[%d] %c M[%d]"
                (temp old `div` 8) (address old `div` 8)
                (opToChar op) (address new `div` 8)
          , printf "\tl.d    $f2, %d($s1)" (address old)
          , printf "\tl.d    $f4, %d($s1)" (address new)
          , fromOp op
          , printf "\ts.d    $f6, %d($s1)\n" (temp new)
          ]
          , temp = nextTemp $ temp old
          , address = temp new
        }


-- | Begins the branch statements
start :: String -> Compiler -> Compiler
start s comp =
    comp { compiled = compiled comp ++ printf "%sStart%d:\n" s (label comp) }

-- | Test for the branch statements (if and while)
test :: String -> Compiler -> Compiler
test s comp =
    comp { compiled = compiled comp ++ unlines [
             printf "\tl.d    $f2, %d($s1)" (address comp)
           , printf "\tl.d    $f4, 0($s1)"
           , printf "\tc.eq.d $f2, $f4"
           , printf "\tbc1t   %s%d\n" s (label comp)
    ]}

-- | Ends the while loop
whileEnd :: Compiler -> Compiler
whileEnd comp =
    comp { compiled = compiled comp ++ unlines [
             printf "\tj      WhileStart%d" (label comp)
           , printf "\nWhileEnd%d:" (label comp)]
           , label = nextLabel $ label comp
    }

-- | Jump between the 'then' and 'else' statements
jump :: Compiler -> Compiler
jump comp =
    comp { compiled = compiled comp ++ unlines[
             printf "\tj      IfEnd%d\n\n" (label comp)
           , printf "Else%d:" (label comp)] }

-- | Ends the if statement
ifEnd :: Compiler -> Compiler
ifEnd comp =
    comp { compiled = compiled comp ++ printf "IfEnd%d:\n" (label comp)
         , label = nextLabel $ label comp }

-- | Converts an Op to an instruction
fromOp :: Op -> String
fromOp Add  = "\tadd.d  $f6, $f2, $f4"
fromOp Sub  = "\tsub.d  $f6, $f2, $f4"
fromOp Mult = "\tmul.d  $f6, $f2, $f4"
fromOp Div  = "\tdiv.d  $f6, $f2, $f4"

-- | Converts an Op to a char
opToChar :: Op -> Char
opToChar Add  = '+'
opToChar Sub  = '-'
opToChar Mult = '*'
opToChar Div  = '/'

-- | Converts a SpecialChar to an instruction
fromSpecial :: SpecialChar -> String
fromSpecial N = "\tla     $a0, NewL"
fromSpecial B = "\tla     $a0, Blank"
fromSpecial T = "\tla     $a0, Tab"

-- | Turn a raw value such as '7' into a memory address
valToAddr :: Char -> Int
valToAddr num = (ord num - ord '0') * 8

-- | Turn a variable such as 'a' into a memory address
varToAddr :: Char -> Int
varToAddr var = (ord var - ord 'a' + 10) * 8

-- | Retrieve the next temporary address
nextTemp :: Int -> Int
nextTemp cur = cur + 8

-- | Retrieve the next label
nextLabel :: Int -> Int
nextLabel = succ

-- | Stuff before the compiled code
preamble :: String
preamble = unlines [
      "# code compiled from a Tiny(r) program"
    , "# preamble"
    , "main:   addu   $s7, $ra, $zero"
    , "        la     $s1, M\n"]

-- | Stuff after the compiled code
postamble :: String
postamble = unlines [
    "# postamble"
    , "\taddu     $ra, $s7, $zero"
    , "\tjr       $ra                     # ret to sys\n"
    , "\t.data"
    , "\t.align   3"
    , "M:      .double  0.,1.,2.,3.,4.,5."
    , "\t.double  6.,7.,8.,9.             # cons"
    , "\t.space   208                     # a to z"
    , "\t.space   1000                    # 125 temps"
    , "Blank:  .asciiz \" \""
    , "NewL:   .asciiz \"\\n\""
    , "Tab:    .asciiz \"\\t\""]
