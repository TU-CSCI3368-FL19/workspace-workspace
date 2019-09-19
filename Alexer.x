{
module Lexer where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
    let  { \s -> LetT } 
    in   { \s -> InT } 
    do   { \s -> DoT } 
    $white+ ;
    done { \s -> DoneT } 
    $alpha [$alpha $digit]* { \s -> VarT s} 
    \=  { \s -> EqT }
    \+  { \s -> PlusT }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = LetT | EqT | InT | IntT Int | VarT String | DoT | DoneT | PlusT deriving (Eq, Show)

scanTokens :: String -> Maybe [Token]
scanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> Just []
                AlexError _ -> Nothing
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap ((act (take len str)):) (go inp')
}
