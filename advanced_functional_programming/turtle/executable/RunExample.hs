module Main where

import           Turtle
import           TurtleExtras
import           TurtleGraphical
import           TurtleSpiral

main = runGraphical' coolSpiral Continuous

treesAndRoad = runGraphical' (p1 <|> p2 <|> p3 <|> p4) Continuous
    where p1 = drawTree
          p2 = wait 16 drawRoad
          p3 = wait 17 (translate v1 . scale 0.5 $ p1)
          p4 = wait 32 (translate v2 . scale 0.25 $ p1)
          v1 = (3/8) *|* (700, 550)
          v2 = (5/8) *|* (700, 500)

reportText = runTextual p1
    where p1 = drawTree

drawTree = resetTurtle (-200, -500) (0, 1) (Just $ Pen Green True) >*> limited 15 (tree 100 (pi/8))

coolSpiral = limited 666 (loop (\x -> forward x >*> left (2*pi/3 + pi/200)) (+3) 1)

drawRoadEdgeUpper = translate (-500, -500) p
    where p = right (atan (1000 / 850)) >*> forward (sqrt (1000^2 + 850^2))

drawRoadEdgeLower = translate (0, -500) p
    where p = right (atan (5 / 7)) >*> forward (sqrt (500^2 + 700^2))

drawRoad = drawRoadEdgeUpper <|> drawRoadEdgeLower

parallel_composition_1 = forever $ left 1 <|> spiralFinite 10 (pi/8)
parallel_composition_2 = lifespan 5 (spiralFinite 10 (pi/8)) <|> limited 10 (forever idle)

test_r_1 = do
    putStrLn "first:"
    runTextual $ (forward 5 <|> backward 10) <|> left 5
    putStrLn "second:"
    runTextual $ forward 5 <|> (backward 10 <|> left 5)

test_r_2 = do
    putStrLn "first:"
    runTextual p
    putStrLn "second:"
    runTextual $ die <|> p
    where p = forward 10

test_r_3 = do
    putStrLn "first:"
    runTextual $ p1 <|> p2
    putStrLn "second:"
    runTextual $ p2 <|> p1
    where p1 = forward 10
          p2 = left 5

test_r_4 = do
  putStrLn "first:"
  runTextual $ idle >*> p
  putStrLn "second:"
  runTextual $ p >*> idle
  where p = limited 10 $ spiralInfinite 1 1

test_r_5 = do
    putStrLn "first:"
    runTextual $ p1 >*> (p2 >*> p3)
    putStrLn "second:"
    runTextual $ (p1 >*> p2) >*> p3
    where p1 = limited 3 $ spiralInfinite 1 1
          p2 = idle
          p3 = times 7 (left (pi/10))
test_r_6 = do
    putStrLn "first:"
    runTextual $ die >*> idle
    putStrLn "second:"
    runTextual die

test_r_7 = do
    putStrLn "first:"
    runTextual $ forever p1 >*> p2
    putStrLn "second:"
    runTextual $ forever p1
    where
        p1 = forward 1
        p2 = backward 1
