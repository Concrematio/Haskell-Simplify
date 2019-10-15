-- Authors:
-- Date:

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp

--------------------------------------------------------------------------------
-- * A1
-- Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should not use String or Char anywhere, since this is
-- not needed.

data Expr = Const Int
          | Bin BinOp Expr Expr
          | Expo Int


ex1 = Bin AddOp (Const 4) (Const 3) -- 4 + 3     = 7
ex2 = Bin MulOp (Const 6) (Const 9) -- 6 * 9     = 54
ex3 = Bin AddOp (ex1) (ex2)         -- 7 + 54    = 61
ex4 = Bin MulOp ex3 (Const 5)       -- 61 * 5    = 305
ex5 = Bin MulOp ex4 (Expo 4)        -- 305 * x^4 = 190625

--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr (Expo n)      = n > 0
prop_Expr (Bin _ e1 e2) = prop_Expr e1 && prop_Expr e2
prop_Expr _             = True


--------------------------------------------------------------------------------
-- * A3
-- Make Expr an instance of Show (along the lines of the example in the lecture)
-- You can use Haskell notation for powers: x^2
-- You should show x^1 as just x.

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Const n)         = show n
showExpr (Bin AddOp e1 e2) = showExpr e1   ++ " + " ++ showExpr e2
showExpr (Bin MulOp e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Expo 1)          = "x"
showExpr (Expo n)          = "x^" ++ show n


showFactor (Const n)         = show n
showFactor (Expo n)          = showExpr (Expo n)
showFactor (Bin AddOp e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showFactor (Bin MulOp e1 e2) =      showFactor e1 ++ " * " ++ showFactor e2
--------------------------------------------------------------------------------
-- * A4
-- Make Expr and instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using
-- quickCheck

-- (Optional)
-- Add a definition of function shrink :: Expr -> [Expr] to Arbitrary
-- which gives hints to quickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests


instance Arbitrary Expr
  where arbitrary = do n <- choose (1,3)
                       rExpr n

rExpr :: Int -> Gen Expr
rExpr 0 = rConst
rExpr n | n>0 = do op <- elements [(Bin MulOp),(Bin AddOp)]
                   l  <- choose (0, n-1)
                   e1 <- rExpr l
                   e2 <- rExpr (n-1 - l)
                   return (op e1 e2)

rConst = do n <- choose (0,9)
            op <- elements [Const, Expo]
            return (op n)

--------------------------------------------------------------------------------
-- * A5
-- Define the eval function which takes a value for x and an expression and
-- evaluates it

eval :: Int -> Expr -> Int
eval x (Expo n)            = x^n
eval _ (Const n)           = n
eval x (Bin MulOp ex1 ex2) = eval x ex1 * eval x ex2
eval x (Bin AddOp ex1 ex2) = eval x ex1 + eval x ex2

--------------------------------------------------------------------------------
-- * A6
-- Define
exprToPoly :: Expr -> Poly
-- Which converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way.

exprToPoly = undefined

-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression

prop_exprToPoly = undefined

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction,
polyToExpr :: Poly -> Expr

polyToExpr = undefined


-- Write (and check) a quickCheck property for this function similar to
-- question 6.
prop_polyToExpr = undefined

--------------------------------------------------------------------------------
-- * A8
-- Write a function
simplify :: Expr -> Expr
-- which simplifies an expression by converting it to a polynomial
-- and back again
simplify = undefined

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
prop_noJunk :: Expr -> Bool

--that checks that a simplified expression does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A7)

prop_noJunk = undefined

--------------------------------------------------------------------------------
