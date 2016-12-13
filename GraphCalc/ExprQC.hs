prop_showReadExpression :: Expr -> Bool
prop_showReadExpression  expr =  (readExpr (showExpr expr)) == expr


prop_simplify :: Expr -> Double -> Bool
prop_simplify expr n = (eval (simplify expr) n) ~== (eval expr n)
