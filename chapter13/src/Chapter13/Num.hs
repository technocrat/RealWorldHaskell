module Chapter13.Num where

import           Data.List

-- | The "operators" that we're going to support
data Op = Plus | Minus | Mul | Div | Pow
          deriving (Eq, Show)

{- | The core symbolic manipulation type. It can be a simple number, a symbol,
   a binary arithmetic operation (such as +), or a unary arithmetic operation
   (such as cos)

   Notice the type of BinaryArith and UnaryArith: it's a recursive type. So,
   we could represent a (+) over two SymbolicManips.
-}
data SymbolicManip a = Number a      -- ^Simple number, such as 5
                     | Symbol String -- ^A symbol, such as x
                     | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
                     | UnaryArith String (SymbolicManip a)
                       deriving Eq

{- | SymbolicManip will be an instance of Num. Define how the Num operations
   are handled over a SymbolManip. This will implement things like (+) for
   SymbolicManip.
-}
instance Num a => Num (SymbolicManip a) where
    a + b       = BinaryArith Plus a b
    a - b       = BinaryArith Minus a b
    a * b       = BinaryArith Mul a b
    negate      = BinaryArith Mul (Number (-1))
    abs         = UnaryArith "abs"
    signum _    = error "signum is unimplemented"
    fromInteger = Number . fromInteger

-- | Make SymbolicManip an instance of Fractional
instance Fractional a => Fractional (SymbolicManip a) where
    a / b        = BinaryArith Div a b
    recip        = BinaryArith Div (Number 1)
    fromRational = Number . fromRational

-- | Make SymbolicManip an instance of Floating
instance Floating a => Floating (SymbolicManip a) where
    pi     = Symbol "pi"
    exp    = UnaryArith "exp"
    log    = UnaryArith "log"
    sqrt   = UnaryArith "sqrt"
    a ** b = BinaryArith Pow a b
    sin    = UnaryArith "sin"
    cos    = UnaryArith "cos"
    tan    = UnaryArith "tan"
    asin   = UnaryArith "asin"
    acos   = UnaryArith "acos"
    atan   = UnaryArith "atan"
    sinh   = UnaryArith "sinh"
    cosh   = UnaryArith "cosh"
    tanh   = UnaryArith "tanh"
    asinh  = UnaryArith "asinh"
    acosh  = UnaryArith "acosh"
    atanh  = UnaryArith "atanh"

-- | Show a SymbolicManip as a String, using conventional algebraic notation
prettyShow :: (Show a, Num a) => SymbolicManip a -> String

-- Show a number or symbol as a bare number or serial
prettyShow (Number x) = show x
prettyShow (Symbol x) = x

prettyShow (BinaryArith op a b) = let pa = simpleParen a
                                      pb = simpleParen b
                                      pop = op2str op
                                  in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) = opstr ++ "(" ++ show a  ++ ")"

op2str :: Op -> String
op2str Plus  = "+"
op2str Minus = "-"
op2str Mul   = "*"
op2str Div   = "/"
op2str Pow   = "**"

{- | Add parentheses where needed. This function is fairly conservative and
   will add parenthesis when not needed in some cases.

   Haskell will have already figured out precedence for us while building up
   the SymbolicManip.
-}
simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith {}) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

-- | Showing a SymbolicManip calls the prettyShow function on it
instance (Show a, Num a) => Show (SymbolicManip a) where
    show = prettyShow

-- | Show a SymbolicManip using RPN. HP calculator users may find this familiar.
rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i = let toList (Number x) = [show x]
                toList (Symbol x) = [x]
                toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
                toList (UnaryArith op a) = toList a ++ [op]
                join :: [a] -> [[a]] -> [a]
                join = intercalate
            in join " " (toList i)

-- | Perform some basic algebraic simplification on a SymblicManip.
simplify :: (Num a, Eq a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) = let sa = simplify ia
                                      sb = simplify ib
                                  in case (op, sa, sb) of
                                       (Mul, Number 1, b)   -> b
                                       (Mul, a, Number 1)   -> a
                                       (Mul, Number 0, _)   -> Number 0
                                       (Mul, _, Number 0)   -> Number 0
                                       (Div, a, Number 1)   -> a
                                       (Plus, a, Number 0)  -> a
                                       (Plus, Number 0, b)  -> b
                                       (Minus, a, Number 0) -> a
                                       _                    -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x

{- | New data type: Units. A Units type contains a number and a SymbolicManip,
   which represents the units of measure. A simple label would be something like
   (Symbol "m")
-}
data Num a => Units a = Units a (SymbolicManip a)
                        deriving Eq

{- | Implement Units for Num. We don't know how to convert between arbitrary
   units, so we generate an error if we try to add numbers with different
   units. For multiplication, generate the appropriate new units.
-}
instance (Num a, Eq a) => Num (Units a) where
    (Units xa ua) + (Units xb ub)
        | ua == ub                = Units (xa + xb) ua
        | otherwise               = error "Mis-matched units in add or subtract"
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua)          = Units (negate xa) ua
    abs (Units xa ua)             = Units (abs xa) ua
    signum (Units xa _)           = Units (signum xa) (Number 1)
    fromInteger i                 = Units (fromInteger i) (Number 1)

-- | Make Units an instance of Fractional
instance (Fractional a, Eq a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a                       = 1 / a
    fromRational r                = Units (fromRational r) (Number 1)

{- | Floating implementation for Units.

   Use some intelligence for angle calculations: support deg and rad
-}
instance (Floating a, Eq a) => Floating (Units a) where
    pi                       = (Units pi (Number 1))
    exp _                    = error "exp not yet implemented in Units"
    log _                    = error "log not yet implemented in Units"
    (Units xa ua) ** (Units xb ub)
        | ub == Number 1     = Units (xa ** xb) (ua ** Number xb)
        | otherwise          = error "units for RHS of ** not supported"
    sqrt (Units xa ua)       = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua)
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise          = error "Units for sin must be deg or rad"
    cos (Units xa ua)
        | ua == Symbol "rad" = Units (cos xa) (Number 1)
        | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise          = error "Units for cos must be deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise          = error "Units for tan must be deg or rad"
    asin (Units xa ua)
        | ua == Number 1     = Units (rad2deg $ asin xa) (Symbol "deg")
        | otherwise          = error "Units for asin must be empty"
    acos (Units xa ua)
        | ua == Number 1     = Units (rad2deg $ acos xa) (Symbol "deg")
        | otherwise          = error "Units for acos must be empty"
    atan (Units xa ua)
        | ua == Number 1     = Units (rad2deg $ atan xa) (Symbol "deg")
        | otherwise          = error "Units for atan must be empty"
    sinh                     = error "sinh not yet implemented in Units"
    cosh                     = error "cosh not yet implemented in Units"
    tanh                     = error "tanh not yet implemented in Units"
    asinh                    = error "asinh not yet implemented in Units"
    acosh                    = error "acosh not yet implemented in Units"
    atanh                    = error "atanh not yet implemented in Units"

{- | A simple function that takes a number and a String and returns an
   appropriate Units type to represent the number and its unit of measure
-}
units :: Num z => z -> String -> Units z
units a b = Units a (Symbol b)

-- | Extract the number only out of a Units type
dropUnits :: Num z => Units z -> z
dropUnits (Units x _) = x

deg2rad :: Floating a => a -> a
deg2rad x = 2 * pi * x / 360

rad2deg :: Floating a => a -> a
rad2deg x = 360 * x / (2 * pi)

{- | Showing units: we show the numeric component, an underscore, then the
   prettyShow version of the simplified units
-}
instance (Show a, Num a, Eq a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)

test :: Num a => a
test = 2 * 5 + 3
