-- 28/05/2011 CodeJam@Cambridge
-- TODO: make instance of Read, understand fully typeclasses.
-- TODO: Understand TokenParser.

import Text.ParserCombinators.Parsec


data SexList = SexSymbol String
    | SexString String
    | SexNumber Integer
    | SexList   [SexList] deriving (Show)

-- functions layer
data TopLevel = Function String Int Lisp
              | TopExpr Lisp

data Lisp = Apply [Int]
          | Let Int Lisp

-- instance Show SexList where
--     show (SexSymbol name) = name
--     show (SexString str) = "\"" ++ str ++ "\""
--     show (SexNumber num) = show num
--     show (SexList lst) = "(" ++ (unwords $ map show lst) ++ ")"

runWith f p input
        = case (parse p "" input) of
            Left err -> error "PARSE"
            Right x  -> f x

eatWhiteShitP = many (oneOf "\r \n\t")
symbolLetterP = char '+' <|> char '*' <|> letter
symbolP = do
              symbol <- many1 symbolLetterP
              eatWhiteShitP
              return $ SexSymbol symbol
stringP = do
            char '"'
            str <- many letter
            char '"'
            eatWhiteShitP
            return $ SexString str

number  = do{ ds <- many1 digit
            ; return (read ds)
            }
        <?> "number"

numberP = do
             num <- number
             eatWhiteShitP
             return $ SexNumber num

exprP = numberP <|> symbolP <|> stringP <|> listP

listP = do
            char '('
            expr <- many exprP
            char ')'
            eatWhiteShitP
            return $ SexList expr

readP = do
            sex <- listP
            eof
            return sex

operator :: String -> (Integer -> Integer -> Integer)
operator "+" = (+)
operator "-" = (-)
operator "*" = (*)

evalSex (SexNumber n) = n
evalSex (SexList []) = error "EVALSEX"
evalSex (SexList ((SexSymbol op) : first : rest)) =
        foldl (operator op) (evalSex first) (map evalSex rest)

-- readEval = evalSex . runWith id readP
