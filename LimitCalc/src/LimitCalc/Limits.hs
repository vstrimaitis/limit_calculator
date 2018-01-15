module LimitCalc.Limits where

import LimitCalc.Point

data Limit a = HasLimit (Point a) | NoLimit | Unknown deriving Show
