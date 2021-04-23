-- -*- haskell -*-
-- This Alex file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
module Syntax.Lex where

import qualified Data.Bits
import Data.Word (Word8)
import Data.Char (ord)
}

$c = [A-Z\192-\221] # [\215]  -- capital isolatin1 letter (215 = \times) FIXME
$s = [a-z\222-\255] # [\247]  -- small   isolatin1 letter (247 = \div  ) FIXME
$l = [$c $s]         -- letter
$d = [0-9]           -- digit
$i = [$l $d _ ']     -- identifier character
$u = [. \n]          -- universal: any character
@rsyms =    -- symbols and non-identifier-like reserved words
   \{ | \} | \. | \* | \/ | \+ | \- | \= | \< | \> | \[ | \, | \] | \; | \( | \) | "in" \:
:-

-- Line comments
"#" [.]* ;

$white+ ;
@rsyms
    { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }
[\\ λ]
    { tok (\p s -> PT p (eitherResIdent (T_Lam . share) s)) }
\∧ | a n d
    { tok (\p s -> PT p (eitherResIdent (T_Conj . share) s)) }
\∨ | o r
    { tok (\p s -> PT p (eitherResIdent (T_Disj . share) s)) }
[\! \¬]
    { tok (\p s -> PT p (eitherResIdent (T_TNot . share) s)) }
\≤ | \< \=
    { tok (\p s -> PT p (eitherResIdent (T_TLeq . share) s)) }
\≥ | \> \=
    { tok (\p s -> PT p (eitherResIdent (T_TGeq . share) s)) }
\⊙ | \( \. \)
    { tok (\p s -> PT p (eitherResIdent (T_TLApp . share) s)) }
\← | \< \-
    { tok (\p s -> PT p (eitherResIdent (T_TSub . share) s)) }
\→ | \- \>
    { tok (\p s -> PT p (eitherResIdent (T_TMatch . share) s)) }

$l $i*
    { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }



$d+ \. $d+ (e (\-)? $d+)?
    { tok (\p s -> PT p (TD $ share s)) }

{

tok :: (Posn -> String -> Token) -> (Posn -> String -> Token)
tok f p s = f p s

share :: String -> String
share = id

data Tok =
   TS !String !Int    -- reserved words and symbols
 | TL !String         -- string literals
 | TI !String         -- integer literals
 | TV !String         -- identifiers
 | TD !String         -- double precision float literals
 | TC !String         -- character literals
 | T_Lam !String
 | T_Conj !String
 | T_Disj !String
 | T_TNot !String
 | T_TLeq !String
 | T_TGeq !String
 | T_TLApp !String
 | T_TSub !String
 | T_TMatch !String

 deriving (Eq,Show,Ord)

data Token =
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

printPosn :: Posn -> String
printPosn (Pn _ l c) = "line " ++ show l ++ ", column " ++ show c

tokenPos :: [Token] -> String
tokenPos (t:_) = printPosn (tokenPosn t)
tokenPos [] = "end of file"

tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p) = p

tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t@(PT p _) = (posLineCol p, tokenText t)

tokenText :: Token -> String
tokenText t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> show s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s
  Err _         -> "#error"
  PT _ (T_Lam s) -> s
  PT _ (T_Conj s) -> s
  PT _ (T_Disj s) -> s
  PT _ (T_TNot s) -> s
  PT _ (T_TLeq s) -> s
  PT _ (T_TGeq s) -> s
  PT _ (T_TLApp s) -> s
  PT _ (T_TSub s) -> s
  PT _ (T_TMatch s) -> s

prToken :: Token -> String
prToken t = tokenText t

data BTree = N | B String Tok BTree BTree deriving (Show)

eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

resWords :: BTree
resWords = b "fst" 19 (b "<" 10 (b "," 5 (b "*" 3 (b ")" 2 (b "(" 1 N N) N) (b "+" 4 N N)) (b "/" 8 (b "." 7 (b "-" 6 N N) N) (b ";" 9 N N))) (b "box" 15 (b "[" 13 (b ">" 12 (b "=" 11 N N) N) (b "]" 14 N N)) (b "false" 17 (b "else" 16 N N) (b "fix" 18 N N)))) (b "out" 29 (b "inR" 24 (b "in:" 22 (b "in" 21 (b "if" 20 N N) N) (b "inL" 23 N N)) (b "next" 27 (b "match" 26 (b "let" 25 N N) N) (b "normal" 28 N N))) (b "true" 34 (b "snd" 32 (b "prevF" 31 (b "prev" 30 N N) N) (b "then" 33 N N)) (b "{" 36 (b "unbox" 35 N N) (b "}" 37 N N))))
   where b s n = let bs = s
                 in  B bs (TS bs n)

unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id
  where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '\\':'r':cs  -> '\r' : unesc cs
    '\\':'f':cs  -> '\f' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
