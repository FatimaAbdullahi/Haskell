{-
Gustav Nicander, Fatima Abdullahi, Mattias Samuelsson  
-}

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
          | AddMul BinOp Expr Expr
          | Pow Int

data Variable = Char


--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr (Pow i) = i < 0
prop_Expr (AddMul _ e1 e2) = prop_Expr e1 && prop_Expr e2
prop_Expr _ = True



--------------------------------------------------------------------------------
-- * A3
-- Make Expr an instance of Show (along the lines of the example in the lecture)
-- You can use Haskell notation for powers: x^2
-- You should show x^1 as just x. 

showExpr :: Expr -> String
showExpr (Const n) = show n
showExpr (AddMul AddOp a b) = showExpr a ++ "+" ++ showExpr b 
showExpr (AddMul MulOp a b) = showFactor a ++ "*" ++ showFactor b
showExpr (Pow 1) = "x"
showExpr (Pow y) = "x^" ++ show y

showFactor :: Expr -> String
showFactor (AddMul AddOp x y) = "(" ++ showExpr (AddMul AddOp x y) ++ ")"
showFactor c                  = showExpr c


instance Show Expr where
   show = showExpr

exExpr = (AddMul MulOp (Pow 4) (AddMul AddOp (Const 7) (Pow 1)))

--------------------------------------------------------------------------------
-- * A4
-- Make Expr and instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using
-- quickCheck

randomex :: Int -> Gen Expr
randomex n = frequency [(1, genConstPow), (n, genOperator)]
  where 
    genConstPow = do
      b <- elements [Const, Pow]
      i <- elements [1..20]
      return (b i)
    
    genOperator = let p = n `div` 2 in do 
      op <- elements [AddOp, MulOp]
      ex1 <- randomex p
      ex2 <- randomex p
      return (AddMul op ex1 ex2)

-- (Optional)
-- Add a definition of function shrink :: Expr -> [Expr] to Arbitrary
-- which gives hints to quickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests

instance Arbitrary Expr
  where arbitrary = sized randomex


--------------------------------------------------------------------------------
-- * A5
-- Define the eval function which takes a value for x and an expression and
-- evaluates it

eval :: Int -> Expr -> Int
eval x expr = case expr of
  (Const n) -> n
  (AddMul MulOp e1 e2) -> eval x e1 * eval x e2
  (AddMul AddOp e1 e2) -> eval x e1 + eval x e2
  (Pow n) -> x ^ n


--------------------------------------------------------------------------------
-- * A6
-- Define
exprToPoly :: Expr -> Poly
-- Which converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way. 

exprToPoly (Const n) = fromList [0, n]
exprToPoly (AddMul AddOp e1 e2) = exprToPoly e1 + exprToPoly e2
exprToPoly (AddMul MulOp e1 e2) = exprToPoly e1 * exprToPoly e2
exprToPoly (Pow n) = fromList (1 : (replicate n 0))

-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression

prop_exprToPoly = undefined

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction, 
polyToExpr :: Poly -> Expr
polyToExpr p = case e of
  [] -> Const 0
  (0 : ys) -> polyToExpr' ys
  (y : []) -> Const y
  (1 : ys) -> add (Pow (length ys)) (polyToExpr' ys)
  (y : ys) -> 
    add
      (mul (Pow (length ys)) (Const y))
      (polyToExpr' ys)
  where
    e = toList p
    polyToExpr' x = polyToExpr (fromList x)

add :: Expr -> Expr -> Expr
add (Const 0) e = e
add e (Const 0) = e
add (Const n) (Const r) = (Const (n + r))
add e1 e2 = AddMul AddOp e1 e2

mul :: Expr -> Expr -> Expr
mul (Const 0) _ = Const 0
mul _ (Const 0) = Const 0
mul (Const 1) e = e
mul e (Const 1) = e
mul (Const n) (Const r) = (Const (n * r))
mul e1 e2 = AddMul MulOp e1 e2


-- Write (and check) a quickCheck property for this function similar to
-- question 6. 
prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr n p = evalPoly n p == eval n (polyToExpr p)

--------------------------------------------------------------------------------
-- * A8
-- Write a function
simplify :: Expr -> Expr
-- which simplifies an expression by converting it to a polynomial
-- and back again
simplify expr = polyToExpr (exprToPoly expr)

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
prop_noJunk :: Expr -> Bool

--that checks that a simplified expression does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A7)

prop_noJunk expr = case e of
  (AddMul _ _ (Const 0)) -> False
  (AddMul _ (Const 0) _) -> False
  (Pow 0)                -> False
  (AddMul MulOp (Const 1) _) -> False
  (AddMul MulOp _ (Const 1)) -> False
  (AddMul _ (Const _) (Const _)) -> False
  (AddMul _ e1 e2) -> prop_noJunk e1 && prop_noJunk e2
  (Const _) -> True
  (Pow _)   -> True
  where
    e = simplify expr

junkExpr :: Expr
junkExpr = (AddMul MulOp (Pow 0) (AddMul AddOp (AddMul AddOp (Const 7) (Const 0)) (Pow 4)))

--------------------------------------------------------------------------------
