module Expr where
import Parsing
import Data.Maybe
import Test.QuickCheck

data Expr = Num Double
          | Var Name
          | Mul Expr Expr
          | Add Expr Expr
          | Sin Expr
          | Cos Expr
          deriving(Eq, Show)

type Name = String

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- instance Show Expr where
--    show = showExpr

{- BNF:
expr      ::= term "+" expr | term.
term      ::= factor "*" term | factor.
factor    ::= number | "(" expr ")" | function.
function  ::= "sin" number | "sin(" expr ")" | "cos" number | "cos(" expr ")"
-}

showExpr :: Expr -> String
showExpr (Add m n) = showTerm m ++ "+" ++ showExpr n
showExpr expr      = showTerm expr

showTerm :: Expr -> String
showTerm (Mul m n) = showFact m ++ "*" ++ showTerm n
showTerm expr      = showFact expr

showFact :: Expr -> String
showFact expr = case expr of
  (Num n) -> show n
  (Var x)  -> "x"
  (Sin n) -> showFunc (Sin n)
  (Cos n) -> showFunc (Cos n)
  otherwise -> "(" ++ showExpr expr ++ ")"

showFunc :: Expr -> String
showFunc expr = case expr of
  (Sin x) -> "sin" ++ showFact x
  (Cos x) -> "cos" ++ showFact x

eval :: Expr -> Double -> Double
eval (Num n) k      = n
eval (Var x) k      = k
eval (Mul m n) k    = (eval m k) * (eval n k)
eval (Add n m) k    = (eval n k) + (eval m k)
eval (Sin n) k      = sin (eval n k)
eval (Cos n) k      = cos (eval n k)



readExpr :: String -> Expr
readExpr input = fst (fromJust (parse expr input))

{-
digit   ::= {1..9}
number  ::= digit{digit}
-}

number :: Parser Double
number = do n <- oneOrMore digit
            return (read n)

{-
char      ::= {a..Z}
string    ::= char{char}
-}

string :: String -> Parser String
string str = do c <- sequence [char s | s <- str]
                return c

{- BNF:
expr      ::= term "+" expr | term.
term      ::= factor "*" term | factor.
factor    ::= number | "(" expr ")" | function.
function  ::= "sin" number | "sin(" expr ")" | "cos" number | "cos(" expr ")"
-}

expr, term, factor, function :: Parser Expr

expr = leftAssoc Add term (char '+')

term = leftAssoc Mul factor (char '*')

factor = (Num <$> readsP) <|> char '(' *> expr <* char ')' <|> function  <|> Var <$> (string "x")

function =  (Sin <$> (string "sin" *> factor)) <|>
            (Cos <$> (string "cos" *> factor))

leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do  i:is <- chain item sep
                            return (foldl op i is)

(~==) :: Double -> Double -> Bool
(~==) x y = (abs(x-y)) <= eps
          where
            eps = 0.001


range = 100

arbExpr :: Int -> Gen Expr
arbExpr size = frequency [(1,rNum), (size, rBin size), (size, rFunc size)]
  where
    rNum = elements [Num n | n<-[1..range]]
    -- rVar =
    rBin size = do op <- elements[Add,Mul]
                   e1 <- arbExpr (size `div` 2)
                   e2 <- arbExpr (size `div` 2)
                   return (op e1 e2)
    rFunc size = do op <- elements[Sin,Cos]
                    e1 <- arbExpr (size `div` 2)
                    return (op e1 )

simplify :: Expr -> Expr
simplify expr = case expr of
  (Num m)         -> Num m
  (Var x)         -> Var "x"
  (Mul (Num 0) _) -> Num 0
  (Mul (Num 1) m) -> simplify m
  (Mul _ (Num 0)) -> Num 0
  (Mul n (Num 1)) -> simplify n
  (Mul n m)       -> Mul (simplify n) (simplify m)
  (Add (Num 0) m) -> simplify m
  (Add n (Num 0)) -> simplify n
  (Add n m)       -> Add (simplify n) (simplify m)
  (Sin n)         -> Sin (simplify n)
  (Cos n)         -> Cos (simplify n)



differentiate :: Expr -> Expr
differentiate expr = (simplify (diff expr))
  where
    diff :: Expr -> Expr
    diff expr = case expr of
      (Var x)   -> Num 1
      (Num n)   -> (Num 0)
      (Mul m n) -> (Add (Mul (diff m) n) (Mul m (diff n)))
      (Add m n) -> Add (diff m) (diff n)
      (Sin n)           ->  Mul(diff n) (Cos (n))
      (Cos n)            -> Mul (diff n)  ((Mul (Num (-1)) (Sin(n))))

-- numbOfVars :: Expr -> Double
-- numbOfVars expr = case expr of
--   (Var "x")         -> 1.0
--   (Num n)           -> 0
--   (Mul (Var "x") m) -> 1.0 + (numbOfVars m)
--   (Mul (Num n) m)   -> numbOfVars m
--   (Mul n m)         -> (numbOfVars n) + (numbOfVars m)
--   (Add _ _)         -> 0.0
