import Data.Char
import Parsing

data SExpr = Ident String | List [SExpr] deriving (Eq, Show)
 
-- sexpr "( f x1 ) y "             = Just ("y ", List [Ident "f", Ident "x1"])
-- sexpr "(  f  ( g  x1 y1)  (h))" = Just ("", List [Ident "f", List [Ident "g",Ident "x1",Ident "y1"], List [Ident "h"]])

-- parse a SExpr
--parseSExpr :: String -> Maybe SExpr
parseSExpr s = case parse parseExpr s of
               []       -> Nothing
               [(e, _)] -> Just e
 
-- parser to parse an input string into a SExpr
--parseExpr :: Parser SExpr
parseExpr = parseBrackets <|> parseIdents

--parseOneIdent :: Parser SExpr
parseOneIdent = parseCharIntIdent <|> parseCharIdent

-- parser for brackets
--parseBrackets :: Parser SExpr
parseBrackets = do symbol "("
                   e <- parseExpr
                   symbol ")"
                   return e

-- parser for Idents
--parseIdents :: Parser [SExpr]
parseIdents = do identList <- some parseOneIdent
                 return (List identList)

-- parser for a single Ident comprising a character followed by an integer
--parseCharIntIdent :: Parser SExpr
parseCharIntIdent = do x <- letter
                       y <- nat
                       let i = [x] ++ (show y)
                       return (Ident i)
  
-- parser for a single Ident comprising solely a character
--parseCharIdent :: Parser SExpr
parseCharIdent = do x <- letter
                    return (Ident [x])