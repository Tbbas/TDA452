import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

import Pages

import Expr

import Data.Maybe



canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw el canvas = do
                          Just c <- (getValue el)
                          g <- (render canvas (stroke (path (points (readExpr c) 0.04 (canWidth, canHeight)))))
                          return g

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
    onEvent scale Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13


points :: Expr -> Double -> (Int,Int) -> [Point]
points expr scale (width,height) = [((fromIntegral x),realToPix (yPos x)) | x <- [0..width]]
              where
                yPos x = eval expr (pixToReal (fromIntegral x))
                pixToReal :: Double -> Double
                pixToReal x = scale * (x- fromIntegral (width `div` 2))

                realToPix :: Double -> Double
                realToPix y = -(y/scale) + fromIntegral (height `div` 2)
