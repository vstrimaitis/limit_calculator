module Derivative where
    import Simplify
    import Expr

    d :: (Num a, Floating a, Eq a) => Expr a -> Expr a
    d (Const _) = Const 0
    d E = Const 0
    d X = Const 1
    d (x :+: y) = simplify $ d x :+: d y
    d (x :-: y) = simplify $ d x :-: d y
    d (x :*: y) = simplify $ x :*: d y :+: y :*: d x
    d (x :/: y) = simplify $ (y :*: d x :-: x :*: d y) :/: (y:*:y)
    d ((:-) x) = Const (-1) :*: simplify (d x)
    d (x :^: y) = simplify $ (x :^: y) :*: d (y :*: Log x)
    d (Sin x) = simplify $ Cos x :*: d x
    d (Cos x) = Const (-1) :*: simplify (Sin x :*: d x)
    d (Log x) = simplify $ Const 1 :/: x :*: d x

    dn :: (Num a, Floating a, Eq a) => Int -> Expr a -> Expr a
    dn n = foldr1 (.) (replicate n d)