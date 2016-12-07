data Expr = Num Float
          | Op Op
          | Func Func

data Op   = Mul Expr Expr
          | Add Expr Expr

data Func = Sin Expr
          | Cos Expr

instance Show Expr where
   show = showExpr

instance Show Op where
   show = showOp

instance Show Func where
   show = showFunc

showExpr :: Expr -> String
showExpr (Num f )= show f
showExpr x    = show x

showOp :: Op -> String
showOp (Mul m n) = show m ++ "*" ++ show n
showOp (Add m n) =
  case m of
    (Op (Mul m' n')) -> case n of
      (Op (Mul x y)) -> "(" ++ show m ++ ")+(" ++ show n ++ ")"
      otherwise -> "(" ++ show m ++ ")+" ++ show n
    otherwise -> case n of
      (Op (Mul x y)) -> show m ++ "+(" ++ show n ++ ")"
      otherwise -> show m ++ "+" ++ show n

showFunc :: Func -> String
showFunc (Sin x) =
    case x of
      (Op (Mul x' y)) -> "Sin(" ++ show x ++ ")"
      (Op (Add x' y)) -> "Sin(" ++ show x ++ ")"
      otherwise -> "Sin" ++ show x
showFunc (Cos x) =
    case x of
      (Op (Mul x' y)) -> "Cos(" ++ show x ++ ")"
      (Op (Add x' y)) -> "Cos(" ++ show x ++ ")"
      otherwise -> "Cos" ++ show x
