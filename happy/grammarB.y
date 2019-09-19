{
module Parser where
--import Lexer

}
%name parse
%tokentype {Token }
%error { parseError}
%monad { Maybe } 

%token
    a        { ATok }
    \*        { MultTok }
    +        { PlusTok }
    b        { BTok }

%left x
%%

S : a E b                   { AEState $2 }
  | x                  { XState }
E : E * E                   { XExp $1 $3 }
  | E + E                   { XExp $1 $3 }
  | b                    { BExp }

{
data Token = DoTok | DoneTok | ThenTok | BTok | XTok | ATok | YTok 
           | MultTok | PlusTok | MinusTok | DivTok | LPTok | RPTok 
           | ITok Int 
data State = AEState Exp | XState deriving (Show, Eq)
data Exp = BExp | XExp Exp Exp deriving (Show, Eq)

parseError :: [Token] -> Maybe a
parseError _ = Nothing
}
