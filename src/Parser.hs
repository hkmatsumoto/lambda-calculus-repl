module Parser
    ( parse'
    )
where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Char

import           Expr

parse' = parse parseExpr ""

parseExpr :: Parser Expr
parseExpr = parseApp <|> parseGrp <|> parseAbs <|> parseVar

parseGrp :: Parser Expr
parseGrp = between (char '(') (char ')') parseExpr

parseVar :: Parser Expr
parseVar = Var <$> letter

parseAbs :: Parser Expr
parseAbs = do
    var  <- char '\\' *> parseVar
    body <- char '.' *> spaces *> parseExpr
    return $ Abs var body

parseTerm :: Parser Expr
parseTerm = parseAbs <|> parseVar <|> parseGrp

parseApp :: Parser Expr
parseApp = chainl1 parseTerm $ spaces >> return App
