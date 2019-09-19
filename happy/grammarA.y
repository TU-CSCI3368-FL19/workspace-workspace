{
module Parser where
--import Lexer
}
%name parse
%tokentype {Token }
%error { parseError}
%monad { Maybe } 

%token
    do       { DoTok }
    done     { DoneTok }
    then     { ThenTok }
    x        { XTok }
    int        { ITok $$ }

%%

S : do E done             { DoDoneS $2 }
  | do E then S           { DoThenS $2 $4 }


E : int                   { IntExp $1 }
  | E x                   { XExp $1 }

{
data Token = DoTok | DoneTok | ThenTok | BTok | XTok | ZTok | YTok 
           | MultTok | PlusTok | MinusTok | DivTok | LPTok | RPTok 
           | ITok Int 
data Stmt = DoDoneS Expr | DoThenS Expr Stmt
            deriving Show
data Expr = XExp Expr | IntExp  Int
            deriving Show
parseError :: [Token] -> Maybe a
parseError _ = Nothing
}
