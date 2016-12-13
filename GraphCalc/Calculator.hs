import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

import Pages

import Expr



canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw elem canvas = do -- how to fill?
  expression  <- eval (readExpr (select elem)) -- reads & evals the expression
  scale       <- case scale of
        isDouble scaleVal  ->  scaleVal
                otherwise -> 0.04
                  where
                    scale = select scale
                    isDouble :: Double -> Bool
                    isDouble num
                      | floor num == ceiling num = True
                      | otherwise = False
  points      <- points expression 0.04 (canWidth,canHeight)


diff :: Elem -> Canvas -> IO ()
diff elem canvas = do -- how to fill?
  expression  <- differentiate (eval (readExpr (select elem))) -- reads & evals the expression
  points      <- points expression 0.04 (canWidth,canHeight)
  -- how to handle bad expression?
  return expression

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    scaling <- mkInput 10 "y"
    scale   <- mkButton "Scale Graph"
    diff    <- mkButton "Differentiate"
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent scale Click $ \_    -> readAndDraw input scaling can
    onEvent diff  Click $ \_    -> diff input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13
