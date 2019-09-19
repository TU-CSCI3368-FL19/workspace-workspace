data Token = IntTok Int | PlusTok | TimesTok 
           | IfTok | ThenTok | ElseTok | TTok | FTok deriving Show

data Exp = PlusExp Exp Exp | TimesExp Exp Exp | IntExp Int 
         | IfExp B Exp Exp deriving Show
data B = T | F deriving Show

--E -> + E E | * E E | int | if B then E else E
--B -> T | F

parseB :: [Token] -> (B, [Token])
parseB (TTok:ts) = (T, ts)
parseB (FTok:ts) = (F, ts)
parseB ts = error ("Invalid token stream: " ++ show ts)

parseE :: [Token] -> (Exp, [Token])
parseE (IfTok:ts) = -- if B then E else E
    let (b, ThenTok:ts') = parseB ts
        (thenE, ElseTok:ts'') = parseE ts'
        (elseE, ts''') = parseE ts''
    in (IfExp b thenE elseE, ts''')
parseE ((IntTok x):ts) = (IntExp x, ts)
parseE (PlusTok:ts) = 
    let (leftE, ts') = parseE ts
        (rightE, ts'') = parseE ts'
    in (PlusExp leftE rightE, ts'')
parseE (TimesTok:ts) = 
    let (leftE, ts') = parseE ts
        (rightE, ts'') = parseE ts'
    in (TimesExp leftE rightE, ts'')
parseE ts =
    let (exp1, ts') = parseE ts
        (PlusTok:ts'') = ts'
        (exp2, ts''') = parseE ts''
    in (PlusExp exp1 exp2, ts''')
