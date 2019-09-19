import Debug.Trace
isFloat :: [Char] -> Bool
isFloat str = 
    let q [] = False
        q (x:xs) = trace ("state: q, letter: " ++ [x]) $
            case x of
                '0' -> q xs
                '1' -> q xs
                '.' -> r xs
                _ -> False
        r [] = False
        r (x:xs) = trace ("state: r, letter: " ++ [x]) $
            case x of
                '0' -> s xs
                '1' -> s xs
                _ -> False
        s [] = True
        s (x:xs) = trace ("state: s, letter: " ++ [x]) $
            case x of
                '0' -> s xs
                '1' -> s xs
                _ -> False
    in q str

isLet ('l':'e':'t':[]) = True
isLet _ = False
