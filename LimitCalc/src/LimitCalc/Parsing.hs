module LimitCalc.Parsing
    ( parseExpr
    , parsePoint
    , ParseError (..)
    ) where

import Prelude hiding (subtract)
import LimitCalc.Expr (Expr)
import qualified LimitCalc.Expr as Expr
import qualified LimitCalc.Limits as Lim
import Data.Foldable (msum)
import Data.Ratio
import Control.Arrow (left)
import Control.Applicative
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

parseExpr :: Fractional a => String -> Either ParseError (Expr a)
parseExpr = left convertError . parse (spaces >> expr) ""

-- Expression parsing

expr :: Fractional a => Parser (Expr a)
expr = chainl1 prod (add <|> subtract) <* spaces
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

x :: Parser ()
x = name "x"

fn :: Fractional a => Parser (Expr a -> Expr a)
fn = msum $ (\(n, f) -> try (name n) >> return f) <$>
    [ ("sin", Expr.Function Expr.Sin)
    , ("cos", Expr.Function Expr.Cos)
    , ("atan", Expr.Function Expr.Atan)
    , ("exp", Expr.Function Expr.Exp)
    , ("ln", Expr.Function Expr.Ln)
    , ("sqrt", Expr.squareRoot)
    ]

name :: String -> Parser ()
name s = string s >> notFollowedBy alphaNum

term :: Fractional a => Parser (Expr a)
term = msum
    [ between (char '(' >> spaces) (spaces >> char ')') expr
    , name "e" >> return (Expr.Function Expr.Exp (Expr.Const 1))
    , name "pi" >> return Expr.Pi
    , fmap Expr.Const num
    , fmap (const Expr.X) x
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
    pwr <- (flip Expr.power <$> (char '^' >> spaces >> negated)) <|> return id
    spaces
    return $ pwr base

negated :: Fractional a => Parser (Expr a)
negated = do
    withSign <- (char '-' >> spaces >> return Expr.negate) <|> return id
    withSign <$> expo

prod :: Fractional a => Parser (Expr a)
prod = chainl1 negated (multiply <|> divide) <* spaces
    where
        multiply = makeOp '*' Expr.Multiply
        divide = makeOp '/' Expr.Divide

makeOp :: Char -> Expr.Op -> Parser (Expr a -> Expr a -> Expr a)
makeOp ch op = char ch >> spaces >> return (Expr.BinaryOp op)

-- Point parsing

parsePoint :: Floating a => String -> Either ParseError (Lim.Point a)
parsePoint = left convertError . parse point ""

point :: Floating a => Parser (Lim.Point a)
point = spaces >> msum
    [ char '+' >> spaces >> name "inf" >> spaces >> return Lim.PositiveInfinity
    , name "inf" >> spaces >> return Lim.PositiveInfinity
    , try (char '-' >> spaces >> name "inf" >> spaces >> return Lim.NegativeInfinity)
    , Lim.Finite <$> value
    ]

value :: Floating a => Parser a
value = spaces >> chainl1 pointProd (add <|> subtract) <* spaces
    where
        add = makePointOp '+' (+)
        subtract = makePointOp '-' (-)

pointTerm :: Floating a => Parser a
pointTerm = msum
    [ between (char '(' >> spaces) (spaces >> char ')') value
    , name "e" >> return (exp 1)
    , name "pi" >> return pi
    , num
    ] <* spaces

pointExpo :: Floating a => Parser a
pointExpo = do
    base <- pointTerm
    spaces
    pwr <- (flip (**) <$> (char '^' >> spaces >> pointNegated)) <|> return id
    spaces
    return $ pwr base

pointNegated :: Floating a => Parser a
pointNegated = do
    withSign <- (char '-' >> spaces >> return negate) <|> return id
    withSign <$> pointExpo

pointProd :: Floating a => Parser a
pointProd = chainl1 pointNegated (multiply <|> divide) <* spaces
    where
        multiply = makePointOp '*' (*)
        divide = makePointOp '/' (/)

makePointOp :: Floating a => Char -> (a -> a -> a) -> Parser (a -> a -> a)
makePointOp ch op = char ch >> spaces >> return op
