{
{-# Options_GHC -w #-}
module Proj.Lexer (scanTokens, Token(..)) where
}

%wrapper "basic"

-- character sets
$digit       = 0-9
$lower       = a-z
$upper       = A-Z
$alpha       = [$lower $upper]
$alphanum    = [$alpha $digit]
$hexit       = [$digit a-f A-F]

-- special characters
$dot         = [ \. ]
$colon       = [ \: ]
$underscore  = [ \_ ]
$tick        = [ `  ]
$fslash      = [ \/ ]
$bslash      = [ \\ ]
$quote       = [ \"" ]

@sym         = $tick [ $dot $colon $alpha ]* [ $dot $colon $underscore $alphanum ]*
@name        = [ $dot $alpha ] [ $alphanum $underscore ]*

@verbs       = "<>"
             | [ \- \+ \* \% \= \< \> \~ \? \. \@ \^ \$ \# \_ \, \! \| \& ] $colon?

@adverbs     = [ $tick  $fslash  $bslash ] $colon?

@sign        = [ \- \+ ]
@decimal     = $digit+
@hexadecimal = $hexit+
@exponent    = e @sign? @decimal
-- @comment     =  (^|[ ]+)/.$

@number      = @decimal [hijef]?
             | @decimal \. @decimal @exponent? [ef]?
             | @decimal @exponent? [ef]?
             | 0x @hexadecimal

@string      = $quote $printable* $quote

token :-
  -- Throw away comments and whitespace
  $white+                 ;
  @sym                    { \s -> TokSym  s   }
  @name                   { \s -> TokName s   }
  @number                 { \s -> TokNum s    }
  @string                 { \s -> TokStr s    }
  @verbs                  { \s -> TokVerb s   }
  @adverbs                { \s -> TokAdverb s }

  $colon                  { \s -> TokAssign }
  "("                     { \s -> TokLParen }
  ")"                     { \s -> TokRParen }
  "{"                     { \s -> TokLBrace }
  "}"                     { \s -> TokRBrace }
  "["                     { \s -> TokLBrack }
  "]"                     { \s -> TokRBrack }
  
{
data Token
  = TokName String
  | TokSym String
  | TokStr String
  | TokNum String
  | TokVerb String
  | TokAdverb String
  | TokAssign
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokLBrack
  | TokRBrack
  deriving (Show, Eq)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
