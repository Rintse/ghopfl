-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Syntax.Raw.Par where
import qualified Syntax.Raw.Abs
import Syntax.Raw.Lex
}

%name pEnvironment Environment
%name pExp Exp
%name pPrg Prg
-- no lexer declaration
%monad { Either String } { (>>=) } { return }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  ',' { PT _ (TS _ 5) }
  '-' { PT _ (TS _ 6) }
  '.' { PT _ (TS _ 7) }
  '/' { PT _ (TS _ 8) }
  ';' { PT _ (TS _ 9) }
  '<' { PT _ (TS _ 10) }
  '=' { PT _ (TS _ 11) }
  '>' { PT _ (TS _ 12) }
  '[' { PT _ (TS _ 13) }
  ']' { PT _ (TS _ 14) }
  '^' { PT _ (TS _ 15) }
  'box' { PT _ (TS _ 16) }
  'boxI' { PT _ (TS _ 17) }
  'else' { PT _ (TS _ 18) }
  'false' { PT _ (TS _ 19) }
  'fix' { PT _ (TS _ 20) }
  'fst' { PT _ (TS _ 21) }
  'if' { PT _ (TS _ 22) }
  'in' { PT _ (TS _ 23) }
  'in:' { PT _ (TS _ 24) }
  'inL' { PT _ (TS _ 25) }
  'inR' { PT _ (TS _ 26) }
  'let' { PT _ (TS _ 27) }
  'match' { PT _ (TS _ 28) }
  'next' { PT _ (TS _ 29) }
  'normal' { PT _ (TS _ 30) }
  'out' { PT _ (TS _ 31) }
  'prev' { PT _ (TS _ 32) }
  'prevI' { PT _ (TS _ 33) }
  'print' { PT _ (TS _ 34) }
  'snd' { PT _ (TS _ 35) }
  'then' { PT _ (TS _ 36) }
  'true' { PT _ (TS _ 37) }
  'unbox' { PT _ (TS _ 38) }
  '{' { PT _ (TS _ 39) }
  '}' { PT _ (TS _ 40) }
  L_Ident  { PT _ (TV $$) }
  L_doubl  { PT _ (TD $$) }
  L_integ  { PT _ (TI $$) }
  L_Lam { PT _ (T_Lam $$) }
  L_Conj { PT _ (T_Conj $$) }
  L_Disj { PT _ (T_Disj $$) }
  L_TNot { PT _ (T_TNot $$) }
  L_TLeq { PT _ (T_TLeq $$) }
  L_TGeq { PT _ (T_TGeq $$) }
  L_TLApp { PT _ (T_TLApp $$) }
  L_TSub { PT _ (T_TSub $$) }
  L_TMatch { PT _ (T_TMatch $$) }

%%

Ident :: { Syntax.Raw.Abs.Ident}
Ident  : L_Ident { Syntax.Raw.Abs.Ident $1 }

Double  :: { Double }
Double   : L_doubl  { (read ($1)) :: Double }

Integer :: { Integer }
Integer  : L_integ  { (read ($1)) :: Integer }

Lam :: { Syntax.Raw.Abs.Lam}
Lam  : L_Lam { Syntax.Raw.Abs.Lam $1 }

Conj :: { Syntax.Raw.Abs.Conj}
Conj  : L_Conj { Syntax.Raw.Abs.Conj $1 }

Disj :: { Syntax.Raw.Abs.Disj}
Disj  : L_Disj { Syntax.Raw.Abs.Disj $1 }

TNot :: { Syntax.Raw.Abs.TNot}
TNot  : L_TNot { Syntax.Raw.Abs.TNot $1 }

TLeq :: { Syntax.Raw.Abs.TLeq}
TLeq  : L_TLeq { Syntax.Raw.Abs.TLeq $1 }

TGeq :: { Syntax.Raw.Abs.TGeq}
TGeq  : L_TGeq { Syntax.Raw.Abs.TGeq $1 }

TLApp :: { Syntax.Raw.Abs.TLApp}
TLApp  : L_TLApp { Syntax.Raw.Abs.TLApp $1 }

TSub :: { Syntax.Raw.Abs.TSub}
TSub  : L_TSub { Syntax.Raw.Abs.TSub $1 }

TMatch :: { Syntax.Raw.Abs.TMatch}
TMatch  : L_TMatch { Syntax.Raw.Abs.TMatch $1 }

BConst :: { Syntax.Raw.Abs.BConst }
BConst : 'true' { Syntax.Raw.Abs.BTrue }
       | 'false' { Syntax.Raw.Abs.BFalse }

Exp12 :: { Syntax.Raw.Abs.Exp }
Exp12 : Ident { Syntax.Raw.Abs.Var $1 }
      | Double { Syntax.Raw.Abs.DVal $1 }
      | Integer { Syntax.Raw.Abs.IVal $1 }
      | BConst { Syntax.Raw.Abs.BVal $1 }
      | '(' Exp ')' { $2 }

Exp11 :: { Syntax.Raw.Abs.Exp }
Exp11 : 'next' Exp12 { Syntax.Raw.Abs.Next $2 }
      | 'prev' '{' Environment '}' '.' Exp12 { Syntax.Raw.Abs.Prev $3 $6 }
      | 'prev' Exp12 { Syntax.Raw.Abs.PrevE $2 }
      | 'prevI' Exp12 { Syntax.Raw.Abs.PrevI $2 }
      | 'box' '{' Environment '}' '.' Exp11 { Syntax.Raw.Abs.Box $3 $6 }
      | 'boxI' Exp12 { Syntax.Raw.Abs.BoxI $2 }
      | 'unbox' Exp12 { Syntax.Raw.Abs.Unbox $2 }
      | 'print' Exp12 { Syntax.Raw.Abs.Print $2 }
      | 'in' Exp12 { Syntax.Raw.Abs.In $2 }
      | 'out' Exp12 { Syntax.Raw.Abs.Out $2 }
      | 'fst' Exp12 { Syntax.Raw.Abs.Fst $2 }
      | 'snd' Exp12 { Syntax.Raw.Abs.Snd $2 }
      | 'inL' Exp12 { Syntax.Raw.Abs.InL $2 }
      | 'inR' Exp12 { Syntax.Raw.Abs.InR $2 }
      | Exp12 { $1 }

Exp10 :: { Syntax.Raw.Abs.Exp }
Exp10 : Exp10 Exp11 { Syntax.Raw.Abs.App $1 $2 }
      | Exp10 TLApp Exp11 { Syntax.Raw.Abs.LApp $1 $2 $3 }
      | Exp11 { $1 }

Exp9 :: { Syntax.Raw.Abs.Exp }
Exp9 : Exp9 '^' Exp10 { Syntax.Raw.Abs.Pow $1 $3 } | Exp10 { $1 }

Exp8 :: { Syntax.Raw.Abs.Exp }
Exp8 : Exp8 '*' Exp9 { Syntax.Raw.Abs.Mul $1 $3 }
     | Exp8 '/' Exp9 { Syntax.Raw.Abs.Div $1 $3 }
     | Exp9 { $1 }

Exp7 :: { Syntax.Raw.Abs.Exp }
Exp7 : Exp7 '+' Exp8 { Syntax.Raw.Abs.Add $1 $3 }
     | Exp7 '-' Exp8 { Syntax.Raw.Abs.Sub $1 $3 }
     | Exp8 { $1 }

Exp6 :: { Syntax.Raw.Abs.Exp }
Exp6 : Exp6 '=' Exp7 { Syntax.Raw.Abs.Eq $1 $3 }
     | Exp6 '<' Exp7 { Syntax.Raw.Abs.Lt $1 $3 }
     | Exp6 '>' Exp7 { Syntax.Raw.Abs.Gt $1 $3 }
     | Exp6 TLeq Exp7 { Syntax.Raw.Abs.Leq $1 $2 $3 }
     | Exp6 TGeq Exp7 { Syntax.Raw.Abs.Geq $1 $2 $3 }
     | Exp7 { $1 }

Exp5 :: { Syntax.Raw.Abs.Exp }
Exp5 : TNot Exp6 { Syntax.Raw.Abs.Not $1 $2 } | Exp6 { $1 }

Exp4 :: { Syntax.Raw.Abs.Exp }
Exp4 : Exp4 Conj Exp5 { Syntax.Raw.Abs.And $1 $2 $3 }
     | Exp4 Disj Exp5 { Syntax.Raw.Abs.Or $1 $2 $3 }
     | Exp5 { $1 }

Exp3 :: { Syntax.Raw.Abs.Exp }
Exp3 : '[' Exp3 ',' Exp3 ']' { Syntax.Raw.Abs.Pair $2 $4 }
     | 'normal' Exp3 { Syntax.Raw.Abs.Norm $2 }
     | Exp4 { $1 }

Exp2 :: { Syntax.Raw.Abs.Exp }
Exp2 : 'if' Exp4 'then' Exp7 'else' Exp7 { Syntax.Raw.Abs.Ite $2 $4 $6 }
     | Exp3 { $1 }

Exp1 :: { Syntax.Raw.Abs.Exp }
Exp1 : 'match' Exp11 '{' Ident TMatch Exp1 ';' Ident TMatch Exp1 '}' { Syntax.Raw.Abs.Match $2 $4 $5 $6 $8 $9 $10 }
     | Exp2 { $1 }

Exp :: { Syntax.Raw.Abs.Exp }
Exp : Lam Ident '.' Exp { Syntax.Raw.Abs.Abstr $1 $2 $4 }
    | 'fix' Ident '.' Exp { Syntax.Raw.Abs.Rec $2 $4 }
    | Exp1 { $1 }

Prg :: { Syntax.Raw.Abs.Prg }
Prg : 'let' Environment 'in:' Exp { Syntax.Raw.Abs.DefProg $2 $4 }
    | Exp { Syntax.Raw.Abs.Prog $1 }

Assignment :: { Syntax.Raw.Abs.Assignment }
Assignment : Ident TSub Exp { Syntax.Raw.Abs.Assign $1 $2 $3 }

Environment :: { Syntax.Raw.Abs.Environment }
Environment : ListAssignment { Syntax.Raw.Abs.Env $1 }

ListAssignment :: { [Syntax.Raw.Abs.Assignment] }
ListAssignment : {- empty -} { [] }
               | Assignment { (:[]) $1 }
               | Assignment ';' ListAssignment { (:) $1 $3 }
{

happyError :: [Token] -> Either String a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer = tokens
}

