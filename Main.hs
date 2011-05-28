-- 28/05/2011 CodeJam@Cambridge
-- TODO: make instance of Read, understand fully typeclasses.
-- TODO: Understand TokenParser.

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P


data SexList = SexSymbol String
    | SexString String
    | SexNumber Integer
    | SexList   [SexList]

instance Show SexList where
    show (SexSymbol name) = name
    show (SexString str) = "\"" ++ str ++ "\""
    show (SexNumber num) = show num
    show (SexList lst) = "(" ++ (unwords $ map show lst) ++ ")"

eatWhiteShitP = many (oneOf "\r \n\t")

symbolP = do
              symbol <- many1 letter
              eatWhiteShitP
              return $ SexSymbol symbol
stringP = do
            char '"'
            str <- many letter                 
            char '"'
            eatWhiteShitP
            return $ SexString str

exprP = symbolP <|> stringP <|> listP

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