{- |
Module      : TurtleExtras
Description : Some derived operators in addition to the turtle language defined in Turtle.hs
-}
module TurtleExtras (
    resetTurtle,
    ready,
    drawRegularPolygon,
    drawSquare,
    drawTriangle,
    drawCircle,
    loop,
    spiral,
    tree,
    transform,
    rotate,
    scale,
    translate,
    (*|*)
    ) where

import Turtle
import TurtleGraphical
import Data.Maybe

-- | reset turtle
resetTurtle :: Location -> Direction -> Maybe Pen -> Program
resetTurtle l d mp = Prog $ \t n ->
    let t'     = t {location = l, direction = d, pen = newPen}
        newPen = fromMaybe (pen t) mp
    in ([Action t' (Op Nothing ("At time " ++ show n ++ ": reset turtle from: " ++ show t ++ ", to: " ++ show t')) n], [t'])

-- | prepared ready to draw a line with specified color
ready :: Color -> Program
ready c = pendown >*> color c
-- | draw a regular polygon with specifed number of edges, length of edge and color of line
drawRegularPolygon :: Int -> Double -> Color -> Program
drawRegularPolygon n d c = ready c >*> times n (forward d) >*> right (2 * pi / fromIntegral n)
-- | draw a square with specified length of edge and color of line
drawSquare :: Double -> Color -> Program
drawSquare = drawRegularPolygon 4
-- | draw a right angle triangle with specified right angle incident edges and color
drawTriangle :: Double -> Double -> Color -> Program
drawTriangle l1 l2 c = ready c >*> forward l1 >*> right (pi - a1) >*> forward l3 >*> right (pi - a2) >*> forward l2
                        where a1 = atan (l2/l1)
                              l3 = sqrt (l1^2 + l2^2)
                              a2 = atan (l1/l2)
-- | draw a circle with specified radius and color
drawCircle :: Double -> Color -> Program
drawCircle r = drawRegularPolygon n d
    where n = 1000
          d = 2 * (r * sin (pi / fromIntegral n))

-- * Higher order functions
--------------------------------------------------------------------------------
-- | given a function (a -> Program), a function (a -> a) and an initial value of type a.
--   return a program that iterates forever over the first function
loop :: (a -> Program) -> (a -> a) -> a -> Program
loop f1 f2 a = f1 a >*> loop f1 f2 (f2 a)

loop' :: (a -> b -> Program) -> (a -> a) -> (b -> b) -> (a, b) -> Program
loop' p f1 f2 (a, b) = p a b >*> loop' p f1 f2 (f1 a, f2 b)
-- | Recreation of spiral program using the loop program
spiral :: Double -> Double -> Program
spiral d a = loop (\x -> forward x >*> right a) (+2) d
-- | Creates tree using loop
tree :: Double -> Double -> Program
tree s d = loop branch (*0.8) s
  where branch s = forward s >*> (left d <|> right d)

-- * Turtle transformations
--------------------------------------------------------------------------------

-- | A transformation of all drawn lines and turtles. Only works propely for
-- linear transformations.
transform :: (Vector -> Vector) -> Program -> Program
transform f (Prog p) = Prog $ \t n ->
  let (as,ts) = p t n
  in (map (transAction f) as, map (transTurtle f) ts)
  where
    transTurtle :: (Vector -> Vector) -> Turtle -> Turtle
    transTurtle f t = t {location = f (location t)}

    transAction :: (Vector -> Vector) -> Action -> Action
    transAction f a = a { turtle = transTurtle f (turtle a), operation = transOp f (operation a)}

    transOp :: (Vector -> Vector) -> Operation -> Operation
    transOp f (Op mg m) = case mg of
        Nothing -> Op mg m
        Just g -> let g' = g {from = f (from g), to = f (to g)} in Op (Just g') m
-- | Rotates a turtle program 'd' radians around the origin.
rotate :: Double -> Program -> Program
rotate d = transform f
  where f (x,y) = (cos d * x + sin d * y, cos d * y - sin d * x)

-- | Scales a program with a factor s
scale :: Double -> Program -> Program
scale s = transform (s *|*)

-- | Translates a program along the vector p
translate :: Vector -> Program -> Program
translate v =  transform (v +|+ )

-- * Some vector operations
--------------------------------------------------------------------------------

infixl 6 *|*
infixl 5 +|+

-- | Multiply scalar with vector
(*|*) :: Double -> Vector -> Vector
d *|* (x, y) = (d*x, d*y)

-- | Add two vectors
(+|+) :: Vector -> Vector -> Vector
(x, y) +|+ (u, v) = (x+u, y+v)
