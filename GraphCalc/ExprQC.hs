import Expr
import Test.QuickCheck
prop_showReadExpression :: Expr -> Double -> Bool
prop_showReadExpression  expr n =  (eval (readExpr (showExpr expr)) n) ~== (eval expr n)


prop_simplify :: Expr -> Double -> Bool
prop_simplify expr n = (eval (simplify expr) n) ~== (eval expr n)
