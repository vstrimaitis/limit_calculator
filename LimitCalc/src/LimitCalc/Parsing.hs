module LimitCalc.Parsing
    ( parseExpr
    , parsePoint
    , ParseError (..)
    ) where

import Prelude hiding (subtract)
import LimitCalc.Ast
import LimitCalc.Point
import qualified LimitCalc.AstPoint as Pt
import Data.Foldable (msum)
import Data.Ratio
import Control.Arrow (left)
import Control.Applicative ((<|>))
import Text.Parsec (parse, try, sourceColumn)
import qualified Text.Parsec as Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error hiding (ParseError)
import Text.Parsec.String (Parser)

data ParseError = ParseError {
    position :: Integer,
    message :: String
} deriving Show

convertError :: Parsec.ParseError -> ParseError
convertError parsecError = ParseError
    { position = fromIntegral $ sourceColumn $ errorPos parsecError
    , message = showErrorMessages
        "or"
        "unknown parse error"
        "expecting"
        "unexpected"
        "end of input"
        (errorMessages parsecError)
    }

parseExpr :: String -> Either ParseError (Expr Rational)
parseExpr = left convertError . parse (spaces >> expr <* eof) ""

-- Expression parsing

expr :: Fractional a => Parser (Expr a)
expr = chainl1 prod (add <|> subtract) <* spaces
    where
        add = makeOp '+' Add
        subtract = makeOp '-' Subtract

num :: Fractional a => Parser a
num = do
    i <- many1 digit
    f <- (char '.' >> many1 digit) <|> return ""
    notFollowedBy alphaNum
    return $ fromRational $ makeNum i f
    where
        makeNum i f = (read i * m + read ('0':f)) % m
            where m = (10 ^ length f) :: Integer

x :: Parser ()
x = name "x"

fn :: Fractional a => Parser (Expr a -> Expr a)
fn = msum $ (\(n, f) -> try (name n) >> return f) <$>
    [ ("sin", Function Sin)
    , ("cos", Function Cos)
    , ("atan", Function Atan)
    , ("arctg", Function Atan)
    , ("exp", Function Exp)
    , ("ln", Function Ln)
    , ("sqrt", Function Sqrt)
    ]

name :: String -> Parser ()
name s = string s >> notFollowedBy alphaNum

term :: Fractional a => Parser (Expr a)
term = msum
    [ between (char '(' >> spaces) (spaces >> char ')') expr
    , name "e" >> return E
    , name "pi" >> return Pi
    , fmap Const num
    , fmap (const X) x
    ] <* spaces

appl :: Fractional a => Parser (Expr a)
appl = msum
    [ fn <* spaces <*> term
    , term
    ]

expo :: Fractional a => Parser (Expr a)
expo = do
    base <- appl
    spaces
    pwr <- (flip (BinaryOp Power) <$> (char '^' >> spaces >> negated)) <|> return id
    spaces
    return $ pwr base

negated :: Fractional a => Parser (Expr a)
negated = do
    withSign <- (char '-' >> spaces >> return Negate) <|> return id
    withSign <$> expo

prod :: Fractional a => Parser (Expr a)
prod = chainl1 negated (multiply <|> divide) <* spaces
    where
        multiply = makeOp '*' Multiply
        divide = makeOp '/' Divide

makeOp :: Char -> Op -> Parser (Expr a -> Expr a -> Expr a)
makeOp ch op = char ch >> spaces >> return (BinaryOp op)

-- Point parsing

parsePoint :: Fractional a => String -> Either ParseError (Point (Pt.Value a))
parsePoint = left convertError . parse (spaces >> point <* eof) ""

point :: Fractional a => Parser (Point (Pt.Value a))
point = spaces >> msum
    [ char '+' >> spaces >> name "inf" >> spaces >> return PositiveInfinity
    , name "inf" >> spaces >> return PositiveInfinity
    , try (char '-' >> spaces >> name "inf" >> spaces >> return NegativeInfinity)
    , Finite <$> value
    ]

value :: Fractional a => Parser (Pt.Value a)
value = spaces >> chainl1 pointProd (add <|> subtract) <* spaces
    where
        add = makePointOp '+' Add
        subtract = makePointOp '-' Subtract

pointTerm :: Fractional a => Parser (Pt.Value a)
pointTerm = msum
    [ between (char '(' >> spaces) (spaces >> char ')') value
    , name "e" >> return Pt.E
    , name "pi" >> return Pt.Pi
    , Pt.Const <$> num
    ] <* spaces

pointExpo :: Fractional a => Parser (Pt.Value a)
pointExpo = do
    base <- pointTerm
    spaces
    pwr <- (flip (Pt.BinaryOp Power) <$> (char '^' >> spaces >> pointNegated)) <|> return id
    spaces
    return $ pwr base

pointNegated :: Fractional a => Parser (Pt.Value a)
pointNegated = do
    withSign <- (char '-' >> spaces >> return Pt.Negate) <|> return id
    withSign <$> pointExpo

pointProd :: Fractional a => Parser (Pt.Value a)
pointProd = chainl1 pointNegated (multiply <|> divide) <* spaces
    where
        multiply = makePointOp '*' Multiply
        divide = makePointOp '/' Divide

makePointOp :: Fractional a => Char -> Op -> Parser (Pt.Value a -> Pt.Value a -> Pt.Value a)
makePointOp ch op = char ch >> spaces >> return (Pt.BinaryOp op)
