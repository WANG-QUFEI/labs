{-# LANGUAGE GADTs #-}
{-|
  Module      : TurtleDeep
  Description : A deep embeeding version of Turtle.hs
-}
module TurtleDeep (
    Program(..)
  , Action(..)
  , Turtle(..)
  , Pen(..)
  , Location
  , Direction
  , Operation(..)
  , Graph(..)
  , Vector
  , Color(..)
  -- primitive operations
  , forward
  , (>*>)
  , (<|>)
  -- running functions
  , runTextual
  , runTurtle
  ) where

import Data.Maybe
-- | definition of types
-- | 2D coordinates
type Vector = (Double, Double)
type Location = Vector
type Direction = Vector
data Color = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White deriving (Enum, Show)
data Pen = Pen {
    penColor :: Color,
    penDown :: Bool
} deriving (Show)
-- | 'Turtle' used to draw graphics
data Turtle = Turtle {
    -- | location of the turtle
    location  :: Location,
    -- | direction to which the turtle faces
    direction :: Direction,
    -- | pen used by the turtle
    pen       :: Pen
} deriving (Show)
-- | Abstraction of one line in graph
data Graph = Graph {
    from       :: Location,
    to         :: Location,
    graphColor :: Color
}
-- | Operation taken
data Operation  = Op {
    -- | the line that might needs to be drawn
    graph   :: Maybe Graph,
    -- | the description of this operation, used for textual interface
    message :: String
}
-- | the definition of time which is used to measure the life of the turtle
type Time = Int
-- | the action a turtle performs in every unit of time
data Action = Action {turtle :: Turtle, operation :: Operation, time :: Time}
-- | Deep embedded version of type Program
data Program where
    Forward  :: Double -> Program
    Backward :: Double -> Program
    Right    :: Double -> Program
    Left     :: Double -> Program
    Color    :: Color -> Program
    Penup    :: Program
    Pendown  :: Program
    Die      :: Program
    Idle     :: Program
    Limited  :: Int -> Program -> Program
    Lifespan :: Int -> Program -> Program
    Times    :: Int -> Program -> Program
    Wait     :: Int -> Program -> Program
    Forever  :: Program -> Program
    (:>*>)   :: Program -> Program -> Program
    (:<|>)   :: Program -> Program -> Program
-- | calculating next position given
move :: Double -> Location -> Direction -> Location
move l (x, y) (x_d, y_d) = (x + x_d * l, y + y_d * l)
-- | calculating new direction given
turn :: Double -> Direction -> Direction
turn a (x_d, y_d) = (x_r * x_d + y_r * y_d, x_r * y_d - y_r * x_d) where (x_r, y_r)  = (cos a, sin a)

forward :: Double -> Program
forward = Forward

(>*>) :: Program -> Program -> Program
(>*>) = (:>*>)

(<|>) :: Program -> Program -> Program
(<|>) = (:<|>)

runProgram :: Turtle -> Time -> Program -> ([Action], [Turtle])
runProgram t n (Forward d) =
    let pos = location t
        nextPos = move d pos (direction t)
        t' = t {location = nextPos }
        op = constructOperation n (pen t) pos nextPos
    in ([Action t' op n], [t'])
runProgram t n ((:>*>) p1 p2) =
    case runProgram t n p1 of
        (as, []) -> (as, [])
        (as, ts) -> (as ++ sortActions as', ts')
            where
                lastTime = time . last $ as
                followingStates = map (\t -> runProgram t (lastTime + 1) p2) ts
                as' = map fst followingStates
                ts' = concatMap snd followingStates
runProgram t n ((:<|>) p1 p2) =
    let (as1, ts1) = runProgram t n p1
        (as2, ts2) = runProgram t n p2
    in (sortActions [as1, as2], ts1 ++ ts2)
-- | a util function intended to be used in TurtleExtras module to implement a function which erases a program, but proved to be unattainable due to the current
-- semantics of operator (>*>). This function, however, is remained.
constructOperation :: Time -> Pen -> Location -> Location -> Operation
constructOperation n p pos nextPos
    | penDown p = let act = case penColor p of
                                White -> "erased"
                                _ -> "drawn"
                            in Op (Just (Graph pos nextPos (penColor p))) ("At time " ++ show n ++ ": A line was " ++ act ++ " from " ++ show pos ++ " to " ++ show nextPos)
    | otherwise = Op Nothing ("At time " ++ show n ++ ": The turtle moved from " ++ show pos ++ " to " ++ show nextPos)


-- | sort the actions by order of the time
sortActions :: [[Action]] -> [Action]
sortActions [] = []
sortActions x = case x' of
        [] -> []
        _ -> let n = time $ head $ head x'
                 pair = map (break ((> n).time)) x'
                 heads = concatMap fst pair
                 tails = map snd pair
             in heads ++ sortActions tails
    where x' = filter (not . null) x

-- | initial turtle state
iniTurtle :: Turtle
iniTurtle = Turtle {
  location  = (0, 0),
  direction = (0,1),
  pen       = Pen { penColor = Black, penDown = True }
}

-- | Run functions
-- | the textual interface that prints what happens in sequential order
runTextual :: Program -> IO ()
runTextual p =
  let (as, _) = runProgram iniTurtle 0 p
  in printMsg as

printMsg :: [Action] -> IO ()
printMsg [] = return ()
printMsg (h:r) = do
    putStrLn . message . operation $ h
    printMsg r
-- | the graphical interface that extracts the actions need to be performed. Used by function 'runGraphical' in module TurtleGraphical
runTurtle :: Program -> [Graph]
runTurtle p =
  let (as, _)  = runProgram iniTurtle 0 p
      graphs   = map (graph . operation) as
  in catMaybes graphs
