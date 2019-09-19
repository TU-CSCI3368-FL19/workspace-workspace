{
module Parser where
import Lexer
--data Token = DoTok | DoneTok | ThenTok | BTok | XTok | ZTok | YTok 
--           | MultTok | PlusTok | MinusTok | DivTok | LPTok | RPTok 
--           | ITok Int 
}

%monad { Maybe } 
%name parse
%tokentype {Token}
%error { parseError}

%token
    x       { XTok }
    y       { YTok }
    z       { ZTok }
    '+'     { PlusTok } 
    '*'     { MultTok }
    '-'     { MinusTok }
    '('     { LPTok }
    ')'     { RPTok }

%left '+' '-' 
%left '*'
%nonassoc NEG
%%

E : E '+' E                     { PlusExp $1 $3}
  | E '*' E                     { MultExp $1 $3}
  | E '-' E                     { MinusExp $1 $3}
  | '-' E %prec NEG                   { NegExp $2}
  | '(' E ')'                   { $2 }
  | x                           { XExp }
  | y                           { YExp }
  | z                           { ZExp }

{

data Expr = XExp | YExp | ZExp | PlusExp Expr Expr | MultExp Expr Expr | MinusExp Expr Expr
          | NegExp Expr
            deriving (Show, Eq)

parseError :: [Token] -> Maybe a
parseError _ = Nothing

}
