-- 28/05/2011 CodeJam@Cambridge

data SexList = SexSymbol String
    | SexString String
    | SexNumber Integer
    | SexList   [SexList]

instance Show SexList where
    show (SexSymbol name) = name
    show (SexString str) = "\"" ++ str ++ "\""
    show (SexNumber num) = show num
    show (SexList lst) = "(" ++ (unwords $ map show lst) ++ ")"
