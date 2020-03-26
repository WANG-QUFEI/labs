{-|
  Module      : TurtleGraphical
  Description : A module with graphical interface to render a turtle program
-}
module TurtleGraphical
    (
      Pace(..),
      runGraphical,
      runGraphical'
    ) where

import Turtle
import Control.Monad
import qualified Graphics.HGL as HGL

data Pace = Slow | Moderate | Fast | VeryFast | Continuous deriving (Enum)

runGraphical :: Program -> IO ()
runGraphical p = runGraphical' p Fast

runGraphical' :: Program -> Pace -> IO ()
runGraphical' p pace = HGL.runGraphics $ do
      w <- HGL.openWindowEx "Turtle!" Nothing (1000, 1000) HGL.DoubleBuffered (Just time)
      (_,(width,height)) <- HGL.getWindowRect w
      HGL.drawInWindow w (HGL.polygon [(0,0),(0,1000),(1000, 1000),(1000,0)])
      onTick w width height graphics
      void(HGL.getKey w)
  where graphics = runTurtle p
        time = case pace of
            Slow -> 1000
            Moderate -> 300
            Fast -> 100
            VeryFast -> 10
            Continuous -> 1

onTick :: HGL.Window -> Int -> Int -> [Graph] -> IO ()
onTick w _ _ []      = return ()
onTick w width height (g:gs)  = do
      HGL.getWindowTick w
      let clr = case graphColor g of
                  Red     -> HGL.Red
                  Green   -> HGL.Green
                  Blue    -> HGL.Blue
                  Black   -> HGL.Black
                  Yellow  -> HGL.Yellow
                  Cyan    -> HGL.Cyan
                  Magenta -> HGL.Magenta
                  White   -> HGL.White
          hgl =  HGL.withColor clr $ HGL.line (fromVector(from g) width height) (fromVector(to g) width height)
      HGL.drawInWindow w hgl
      onTick w width height gs

fromVector :: Vector -> Int -> Int -> (Int,Int)
fromVector (x,y) w h = (div w 2 + floor x, div h 2 - floor y)
