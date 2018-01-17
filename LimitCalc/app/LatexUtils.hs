module LatexUtils where

parens :: Integer -> Integer -> String -> String
parens a b val
    | a < b = "(" ++ val ++ ")"
    | otherwise = val

showDouble :: Double -> String
showDouble x = fixString (show x)
    where
        fixString s
            | endsWithZero s = take (length s - 2) s
            | otherwise = s

        endsWithZero ".0" = True
        endsWithZero (_:xs) = endsWithZero xs
