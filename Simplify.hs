module Simplify where

    import Expr
    import ExtNum

    simp :: (Num a, Eq a, Floating a) => Expr a -> Expr a
    simp (Const x :+: Const y) = Const (x + y)
    simp (x :+: Const 0) = simp x
    simp (Const 0 :+: x) = simp x

    simp (Const x :-: Const y) = Const (x - y)
    simp (x :-: Const 0) = simp x
    simp (Const 0 :-: x) = simp ((:-) x)

    simp (Const x :*: Const y) = Const (x*y)
    simp (x :*: Const 1) = simp x
    simp (Const 1 :*: x) = simp x
    simp (x :*: Const 0) = Const 0 -- ?
    simp (Const 0 :*: x) = Const 0 -- ?

    simp (Const x :^: Const y) = Const (x**y)
    simp (x :^: Const 1) = simp x
    simp (x :^: Const 0) = Const 1

    simp (Const x :/: Const y) = Const (x/y)
    simp (Const 0 :/: x) = Const 0
    simp (x :/: Const 1) = simp x
    simp (x :/: y) | x ==y = Const 1
    simp ((x:^:c):/:y) | x == y = x:^:(c:-:Const 1)
    simp (x :*: (Const y :/: z)) = Const y :*: simp (x :/: z)

    simp ((:-) (Const x)) = Const (-x)
    simp ((:-) ((:-) x)) = simp x

    simp ((z :^: Const x) :^: Const y) = z :^: Const (x * y)

    simp (Const x :*: (Const y :*: z)) = Const (x*y) :*: simp z
    simp (Const x :*: z :*: Const y) = Const (x*y) :*: simp z
    simp (z :*: Const x :*: Const y) = Const (x*y) :*: simp z

    simp (Const x :*: (y :+: z)) = (Const x :*: simp y) :+: (Const x :*: simp z)
    simp ((y :+: z) :*: Const x) = (Const x :*: simp y) :+: (Const x :*: simp z)

    simp (x :+: y) = simp x :+: simp y
    simp (x :-: y) = simp x :-: simp y
    simp (x :*: y) = simp x :*: simp y
    simp (x :/: y) = simp x :/: simp y
    simp (x :^: y) = simp x :^: simp y
    simp (Sin x) = Sin (simp x)
    simp (Cos x) = Cos (simp x)
    simp (Log x) = Log (simp x)
    simp x = x

    

    simplify expr = simplify' expr (Const 0)
        where simplify' expr last
                | expr == last = expr
                | otherwise = simplify' (simp expr) expr







    sample = X :^: X :*: (X :*: ((Const 1.0 :/: X) :*: Const 1.0) :+: Const 1.0  :*: Log X)