module Derivative where

infixl 4 :+:, :-:
infixl 5 :*:, :/:
infixl 6 :^:

data Expr a
    = X
    | Const a
    | (Expr a) :+: (Expr a)
    | (Expr a) :-: (Expr a)
    | (Expr a) :*: (Expr a)
    | (Expr a) :/: (Expr a)
    | (Expr a) :^: (Expr a)
    deriving (Eq, Show)

simp :: (Floating a, Eq a) => Expr a -> Expr a
simp (Const x :+: Const y) = Const (x + y)
simp (x :+: Const 0) = simp x
simp (Const 0 :+: x) = simp x

simp (Const x :-: Const y) = Const (x - y)
simp (x :-: Const 0) = simp x
simp (Const 0 :-: x) = Const (-1) :*: simp x

simp (Const x :*: Const y) = Const (x*y)
simp (x :*: Const 1) = simp x
simp (Const 1 :*: x) = simp x
simp (x :*: Const 0) = Const 0
simp (Const 0 :*: x) = Const 0

simp (Const x :^: Const y) = Const (x**y)
simp (x :^: Const 1) = simp x
simp (x :^: Const 0) = Const 1

simp ((z :^: Const x) :^: Const y) = z :^: Const (x * y)

simp (Const x :*: (Const y :*: z)) = Const (x*y) :*: simp z
simp (Const x :*: z :*: Const y) = Const (x*y) :*: simp z
simp (z :*: Const x :*: Const y) = Const (x*y) :*: simp z

simp (x :+: y) = simp x :+: simp y
simp (x :-: y) = simp x :-: simp y
simp (x :*: y) = simp x :*: simp y
simp (x :/: y) = simp x :/: simp y
simp (x :^: y) = simp x :^: simp y
simp x = x

simplify expr = simplify' expr (Const 0)
    where simplify' expr last
            | expr == last = expr
            | otherwise = simplify' (simp expr) expr


d :: (Floating a, Eq a) => Expr a -> Expr a
d (Const _) = Const 0
d X = Const 1
d (x :+: y) = simplify $ d x :+: d y
d (x :-: y) = simplify $ d x :-: d y
d (x :*: y) = simplify $ x :*: d y :+: y :*: d x
d (x :/: y) = simplify $ (y :*: d x :-: x :*: d y) :/: (y:^:Const 2)
d (x :^: Const n) = Const n :*: x :^: Const (n-1) :*: d x
d (x :^: _) = error "Cannot take derivative of non constant power"

dn :: (Floating a, Eq a) => Int -> Expr a -> Expr a
dn 0 = id
dn n = foldr1 (.) (replicate n d)


eval :: (Floating a) => Expr a -> a -> a
eval (Const x) _ = x
eval X a = a
eval (x :+: y) a = eval x a + eval y a
eval (x :-: y) a = eval x a - eval y a
eval (x :*: y) a = eval x a * eval y a
eval (x :/: y) a = eval x a / eval y a
eval (x :^: y) a = eval x a ** eval y a