module Compile where

import Parser
import Data.Char (ord)
import Text.Printf (printf)

data Compiler = Compiler {
    code    :: String,
    address :: Int,
    temp    :: Int,
    label   :: Int
} deriving (Show)

compile :: [Statement] -> Compiler
compile = foldl compileStatement Compiler { code = preamble, address = 0
                                            , temp = 288, label = 0}

compileStatement :: Compiler -> Statement -> Compiler
compileStatement comp s = case s of
    Assignment var expr ->
        let comp' = expression comp expr in
        comp' { code = code comp' ++ unlines [
                  printf "# M[%d] = M[%d]"
                   (varToAddr var `div` 8) (address comp' `div` 8)
                , printf "\tl.d    $f2, %d($s1)" (address comp')
                , printf "\ts.d    $f2, %d($s1)\n" (varToAddr var)]
        }
    Print (Expr expr) ->
        let comp' = expression comp expr in
        comp' { code = code comp' ++ unlines [
                  printf "# print value"
                , printf "\tli     $v0,  3"
                , printf "\tl.d    $f12, %d($s1)" (address comp')
                , printf "\tsyscall\n"]
        }
    Print (Special sp) ->
        comp { code = code comp ++ unlines [
                 "# print special character"
               , "\tli     $v0, 4"
               , fromSpecial sp
               , "\tsyscall\n"]
        }
    Get var ->
        comp { code = code comp ++ unlines [
                 printf "# read M[%d] as double" (varToAddr var `div` 8)
               , printf "\tli     $v0, 7"
               , printf "\tsyscall"
               , printf "\ts.d    $f0, %d($s1)\n" (varToAddr var)]
        }
    Cond (IfThen expr ss) ->
        ifEnd
        . elseLabel
        . compileJump
        . (\c -> foldl compileStatement c ss)
        . elseJump
        . test $ expression (ifStart comp) expr
    Cond (IfThenElse expr ss ss') ->
        ifEnd
        . (\c -> foldl compileStatement c ss')
        . elseLabel
        . compileJump
        . (\c -> foldl compileStatement c ss)
        . elseJump
        . test $ expression (ifStart comp) expr
    Loop (While expr ss) ->
        whileEnd
        . (\c -> foldl compileStatement c ss)
        . while $ expression (whileStart comp) expr
    Terminate -> comp { code = code comp ++ postamble }

factor :: Compiler -> Factor -> Compiler
factor comp f = case f of
    Factor expr -> expression comp expr
    Var var -> comp { address = varToAddr var }
    Val val -> comp { address = valToAddr val }

expression :: Compiler -> Expression -> Compiler
expression comp expr = case expr of
    Expression t [] -> term comp t
    Expression t subs -> let out' = term comp t
                         in foldl subExpression out' subs

subExpression :: Compiler -> SubExpression -> Compiler
subExpression comp (SubExpression op t) =
    let comp' = term comp t in
    updateCompiler comp comp' op

term :: Compiler -> Term -> Compiler
term comp t = case t of
    Term f []   -> factor comp f
    Term f subs -> let comp' = factor comp f
                   in foldl subTerm comp' subs

subTerm :: Compiler -> SubTerm -> Compiler
subTerm comp (SubTerm op f) =
    let comp' = factor comp f in
    updateCompiler comp comp' op

updateCompiler :: Compiler -> Compiler -> Op -> Compiler
updateCompiler old new op =
    new { code = code old ++
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

--
-- While
--
whileStart :: Compiler -> Compiler
whileStart comp =
    comp { code = code comp ++ printf "WhileStart%d:\n" (label comp) }

while :: Compiler -> Compiler
while comp =
    comp { code = code comp ++ unlines [
             printf "\tl.d    $f2, %d($s1)" (address comp)
           , printf "\tl.d    $f4, 0($s1)"
           , printf "\tc.eq.d $f2, $f4"
           , printf "\tbc1t   WhileEnd%d\n" (label comp)
        ]}

whileEnd :: Compiler -> Compiler
whileEnd comp =
    comp { code = code comp ++ unlines [
             printf "\tj      WhileStart%d" (label comp)
           , printf "\nWhileEnd%d:" (label comp)]
           , label = nextLabel $ label comp
    }
--
-- If
--
ifStart :: Compiler -> Compiler
ifStart comp =
    comp { code = code comp ++ printf "IfStart%d:\n" (label comp) }

test :: Compiler -> Compiler
test comp =
    comp { code = code comp ++ unlines [
             printf "\tl.d    $f2, %d($s1)" (address comp)
           , printf "\tl.d    $f4, 0($s1)"
           , printf "\tc.eq.d $f2, $f4"
    ]}

elseJump :: Compiler -> Compiler
elseJump comp =
    comp { code = code comp ++ printf "\tbc1t   Else%d\n\n" (label comp) }

compileJump :: Compiler -> Compiler
compileJump comp =
    comp { code = code comp ++ printf "\tj      IfEnd%d\n\n" (label comp) }

elseLabel :: Compiler -> Compiler
elseLabel comp =
    comp { code = code comp ++ printf "Else%d:\n" (label comp) }

ifEnd :: Compiler -> Compiler
ifEnd comp =
    comp { code = code comp ++ printf "IfEnd%d:\n" (label comp)
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

-- | Retrieve the next lable
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
