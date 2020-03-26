{- |
    Module : TurtleSpiral
    Description : A module which contains functions to create turtle programs that draw infinite and finite spirals.
-}
module TurtleSpiral (
      spiralInfinite
    , spiralFinite
    , spiralFinite'
    , spiralTwice
    , spiralTwicePar
    ) where

import Turtle

-- | draw an infinite spiral which forwards 'd' distance and turn 'a' degree on the right in each step
-- d : the distance to forward; a : angle to turn in each step
spiralInfinite :: Double -> Double -> Program
spiralInfinite d a = forward d >*> right a >*> spiralInfinite (d + 2) a

-- | draw a finite spiral which stops once the forward distance exceeds 100
spiralFinite :: Double -> Double -> Program
spiralFinite d a | d > 100 = idle
                 | otherwise = forward d >*> right a >*> spiralFinite (d + 2) a

-- | define a limited version in terms of the infinite one
spiralFinite' :: Double -> Double -> Program
spiralFinite' d a = limited ((102 - fromEnum d) `div` 2) $ spiralInfinite d a

-- | draw a finite spiral and when done starts drawing an infinite spiral where the finite spiral ended
spiralTwice :: Double -> Double -> Program
spiralTwice d a = spiralFinite d a >*> spiralInfinite 102 a

-- | draw a finite spiral and in parallel draw a infinite spiral where the finite spiral ended
spiralTwicePar :: Double -> Double -> Program
spiralTwicePar d a = spiralInfinite d a <|> spiralInfinite (d + 50) (a + (pi / 4))
