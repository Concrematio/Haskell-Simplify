-- Authors: Magnus Eld, Ida Franzén Karlsson, Viktor Hauge
-- Date: 2019-10-18

import Poly
import Test.QuickCheck

-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp

--------------------------------------------------------------------------------
-- * A1
-- An expression can consists additions and multiplications
-- of numbers and x raised to a power
data Expr = Const Int
          | Bin BinOp Expr Expr
          | Expo Int

--------------------------------------------------------------------------------
-- * A2
-- Property of Expr: Exponents should never be negative
prop_Expr :: Expr -> Bool
prop_Expr (Expo n)      = n >= 0
prop_Expr (Bin _ e1 e2) = prop_Expr e1 && prop_Expr e2
prop_Expr _             = True

--------------------------------------------------------------------------------
-- * A3

instance Show Expr where
  show = showExpr

-- Show function for expressions
showExpr :: Expr -> String
showExpr (Const n)         = show n
showExpr (Expo 1)          = "x"
showExpr (Expo n)          = "x^" ++ show n
showExpr (Bin AddOp e1 e2) = showExpr e1   ++ " + " ++ showExpr e2
showExpr (Bin MulOp e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
  where
    showFactor e@(Bin AddOp _ _) = "(" ++ showExpr e ++ ")"
    showFactor e                 = showExpr e

--------------------------------------------------------------------------------
-- * A4
-- Expr is an instance of Arbitrary
instance Arbitrary Expr
  where
    arbitrary = rExpr =<< choose (1,4)
      where
        rExpr :: Int -> Gen Expr
        rExpr 0 = do n <- choose (-9,9)
                     elements [Const n, Expo (abs n)]

        rExpr n = do l  <- choose (0, n-1)
                     e1 <- rExpr l
                     e2 <- rExpr (n-1 - l)
                     elements [Bin MulOp e1 e2, Bin AddOp e1 e2]

--------------------------------------------------------------------------------
-- * A5
-- Evaluates an expression with a given value of x
eval :: Int -> Expr -> Int
eval x (Expo n)          = x^n
eval _ (Const n)         = n
eval x (Bin MulOp e1 e2) = eval x e1 * eval x e2
eval x (Bin AddOp e1 e2) = eval x e1 + eval x e2

--------------------------------------------------------------------------------
-- * A6
-- Converts an expression to a polynomial
exprToPoly :: Expr -> Poly
exprToPoly (Const n)         = fromList [n]
exprToPoly (Expo n)          = fromList (1:replicate n 0)
exprToPoly (Bin AddOp e1 e2) = exprToPoly e1 + exprToPoly e2
exprToPoly (Bin MulOp e1 e2) = exprToPoly e1 * exprToPoly e2

-- Property of exprToPoly: evaluating an expression should be equal to
-- converting the expression to a polynomial and then evaluating it
prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly x e = eval x e == evalPoly x (exprToPoly e)

--------------------------------------------------------------------------------
-- * A7
-- Converts a polynomial to an expression
polyToExpr :: Poly -> Expr
polyToExpr poly = toExpr 0 $ reverse $ toList poly
  where
    toExpr _ []     = Const 0 -- toList (fromList [0]) returns []!
    toExpr 0 [x]    = Const x
    toExpr n (0:xs) = toExpr (n+1) xs
    toExpr 0 (x:xs) = Bin AddOp (Const x) (toExpr 1 xs)
    toExpr n [x]    = Bin MulOp (Const x) (Expo n)
    toExpr n (x:xs) = Bin AddOp (Bin MulOp (Const x) (Expo n)) (toExpr (n+1) xs)

-- Property of polyToExpr: evaluating a plynomial should be equal to
-- converting the polynomial to an expression and then evaluating it
prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr x poly = eval x (polyToExpr poly) == evalPoly x poly

--------------------------------------------------------------------------------
-- * A8
-- Simplifies an expression by collecting terms
simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

--------------------------------------------------------------------------------
-- * A9
-- Property of a simplified expression:
-- Should not contain multiplication by one or zero, addition of zero,
-- addition or multiplication of constants, or x to the power of zero
prop_noJunk :: Expr -> Bool
prop_noJunk expr = noJunk (simplify expr)
  where
    noJunk (Bin _ (Const n) (Const m))
        | n == 0 || m == 0 = False

    noJunk (Bin MulOp (Const n) (Const m))
        | n == 1 || m == 1 = False

    noJunk (Bin _ e1 e2) = prop_noJunk e1 || prop_noJunk e2
    noJunk (Expo 0)        = False
    noJunk _               = True

--------------------------------------------------------------------------------
