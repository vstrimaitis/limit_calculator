module Parsing
    ( parseExpr
    -- , convertError
    -- , ParseError
    ) where

import Expr (Expr)
import qualified Expr
import Data.Foldable (msum)
import Data.List (genericLength)
import Data.Ratio
import Control.Arrow (left)
import Control.Applicative
import Text.Parsec (parse, ParseError)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

data ParseError = ParseError {
    position :: Integer,
    message :: String
} deriving Show

-- convertError :: a -> ParseError
-- convertError _ = ParseError { position = 0, message = "parse failed" }

parseExpr :: Fractional a => String -> Either Text.Parsec.ParseError (Expr a)
parseExpr = {- left convertError . -} parse expr ""

expr :: Fractional a => Parser (Expr a)
expr = spaces >> chainl1 prod (spaces >> (add <|> subtract))
    where
        add = makeOp '+' Expr.Add
        subtract = makeOp '-' Expr.Subtract

num :: Fractional a => Parser a
num = do
    i <- many1 digit
    f <- (char '.' >> many1 digit) <|> return ""
    notFollowedBy alphaNum
    return $ fromRational $ makeNum i f
    where
        makeNum i f = (read i * m + read ('0':f)) % m
            where m = (10 ^ length f) :: Integer

expon :: Fractional a => Parser (Either Integer a)
expon = do
    sign <- (char '-' >> return (-1)) <|> return 1
    spaces
    n <- (num :: Parser Rational)
    return $ case denominator n of
        1 -> Left $ numerator n
        _ -> Right $ fromRational n

x :: Parser ()
x = name "x"

fn :: Parser Expr.Fn
fn = msum $ (\(n, f) -> name n >> return f) <$>
    [ ("sin", Expr.Sin)
    , ("cos", Expr.Cos)
    , ("atan", Expr.Atan)
    , ("exp", Expr.Exp)
    , ("ln", Expr.Ln)
    ]

name :: String -> Parser ()
name s = string s >> notFollowedBy alphaNum

term :: Fractional a => Parser (Expr a)
term = msum
    [ between (char '(' >> spaces) (spaces >> char ')') expr
    , fmap Expr.Const num
    , fmap (const Expr.X) x
    ] <* spaces

appl :: Fractional a => Parser (Expr a)
appl = msum
    [ Expr.Function <$> fn <* spaces <*> term
    , pure Expr.negate <* char '-' <* spaces <*> appl
    , term
    ]

expo :: Fractional a => Parser (Expr a)
expo = do
    f <- appl
    t <- fmap Just (spaces >> char '^' >> spaces >> expon) <|> return Nothing
    return $ case t of
        Just (Left int) -> Expr.IntegerPower f int
        Just (Right x) -> Expr.Power f x
        Nothing -> f

prod :: Fractional a => Parser (Expr a)
prod = chainl1 expo (spaces >> (multiply <|> divide))
    where
        multiply = makeOp '*' Expr.Multiply
        divide = makeOp '/' Expr.Divide

makeOp :: Char -> Expr.Op -> Parser (Expr a -> Expr a -> Expr a)
makeOp ch op = char ch >> spaces >> return (Expr.BinaryOp op)