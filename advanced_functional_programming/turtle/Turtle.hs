{-|
  Module      : Turtle
  Description : A EDSL which defines the essential data types and funcitons to implement the 'turtule Graph'functionality.
-}
module Turtle (
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
  , right
  , color
  , penup
  , pendown
  , die
  , limited
  , (>*>)
  , (<|>)
  -- derived operations
  , backward
  , left
  , idle
  , lifespan
  , times
  , forever
  , wait
  -- running functions
  , runTextual
  , runTurtle
  ) where

import Data.Maybe
-- | definition of types
-- | 2D coordinates
type Vector = (Double, Double)
-- | location on the board
type Location = Vector
-- | direction on the board
type Direction = Vector
-- | color of pen
data Color = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White deriving (Enum, Show)
-- | pen in turtle
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

-- | a 'turtle program' is a function which returns a list of action and turtle, given a turtle and an initial step number
newtype Program = Prog {recipe :: Turtle -> Time -> ([Action], [Turtle])}

-- | calculating next position given
move :: Double -> Location -> Direction -> Location
move l (x, y) (x_d, y_d) = (x + x_d * l, y + y_d * l)

-- | calculating new direction given
turn :: Double -> Direction -> Direction
turn a (x_d, y_d) = (x_r * x_d + y_r * y_d, x_r * y_d - y_r * x_d) where (x_r, y_r)  = (cos a, sin a)

-- | Basic constructors of Program
-- | move forward the given length
forward :: Double -> Program
forward l = Prog $ \t n ->
  let pos = location t
      nextPos = move l pos (direction t)
      t' = t {location = nextPos }
      op = constructOperation n (pen t) pos nextPos
  in ([Action t' op n], [t'])

-- | a util function intended to be used in TurtleExtras module to implement a function which erases a program, but proved to be unattainable due to the current
-- semantics of operator (>*>). This function, however, is remained.
constructOperation :: Time -> Pen -> Location -> Location -> Operation
constructOperation n p pos nextPos
    | penDown p = let act = case penColor p of
                                White -> "erased"
                                _ -> "drawn"
                            in Op (Just (Graph pos nextPos (penColor p))) ("At time " ++ show n ++ ": A line was " ++ act ++ " from " ++ show pos ++ " to " ++ show nextPos)
    | otherwise = Op Nothing ("At time " ++ show n ++ ": The turtle moved from " ++ show pos ++ " to " ++ show nextPos)

-- | move backward the given length
backward :: Double -> Program
backward l = forward (-l)

-- | make the turtle turn left the given degree
right :: Double -> Program
right a
 | a == 0    = Prog $ \t n -> ([Action t (Op Nothing ("At time " ++ show n ++ ": The turtle is idle")) n], [t])
 | otherwise = Prog $ \t n ->
                  let dir     = direction t
                      nextDir = turn a dir
                      dir' = if a > 0 then "right" else "left"
                      message   = "At time " ++ show n ++ ": The turtle turned " ++ show (  abs a) ++ " degrees to the " ++ dir' ++ " from " ++ show dir ++ " to " ++ show nextDir
                      t'    = t { direction = nextDir }
                  in ([Action t' (Op Nothing message) n], [t'])

-- | make the turtle turn right the given degree
left :: Double -> Program
left a = right (-a)

-- | change the color of the turtle's pen
color :: Color -> Program
color c = Prog $ \t n ->
  let
      clr = penColor $ pen t
      message = "At time " ++ show n ++ ": The turtle's pen switched colour from " ++ show clr ++ " to " ++ show c
      t'  = t { pen = (pen t) { penColor = c }}
  in ([Action t' (Op Nothing message) n], [t'])

-- | lift the pen of the turtle
penup :: Program
penup = Prog $ \t n ->
  let message = "At time " ++ show n ++ ": Lifted the pen up"
      t' = t { pen = (pen t) { penDown = False }}
  in ([Action t' (Op Nothing message) n], [t'])

-- | put the pen of the turtle down
pendown :: Program
pendown = Prog $ \t n ->
  let message = "At time " ++ show n ++ ": Put the pen down"
      t'  = t { pen = (pen t) { penDown = True }}
  in ([Action t' (Op Nothing message) n], [t'])

-- | kill the turtle
die :: Program
die = Prog $ \t n ->
  let message = "At time " ++ show n ++ ": The turtle was killed"
  in ([Action t (Op Nothing message) n], [])

-- | the turtle is idle
idle :: Program
idle = left 0

-- | Combinators
-- | makes the turtle stop what it is doing after a specified period of time
limited :: Int -> Program -> Program
limited l (Prog r) = Prog $ \t n ->
    let (as, ts) = r t n
    in case takeWhile ((< l + n) . time) as of
        [] -> ([], ts)
        as' -> let lastTime = (time . last) as'
                   ts' = map turtle $ dropWhile ((< lastTime) . time) as'
               in (as', ts')

-- | kills the turtle after a specified period of time,
lifespan :: Int -> Program -> Program
lifespan n p = limited n p >*> die

-- | repeats a turtle program a certain number of times
times :: Int -> Program -> Program
times 0 p = idle
times m p = p >*> times (m - 1) p

-- | wait certain amount of time to execute the other program
wait :: Int -> Program -> Program
wait n p = times n idle >*> p

-- | repeats a program forever
forever :: Program -> Program
forever p = p >*> forever p

-- | perform program one after another, namely, sequential operation of programs
(>*>) :: Program -> Program -> Program
(Prog r1) >*> (Prog r2) = Prog $ \t n ->
    case r1 t n of
        (as, []) -> (as, [])
        (as, ts) -> (as ++ sortActions as', ts')
            where
                lastTime = time . last $ as
                followingStates = map (`r2` (lastTime + 1)) ts
                as' = map fst followingStates
                ts' = concatMap snd followingStates

-- |  perform several programs in parallel
(<|>) :: Program -> Program -> Program
(Prog r1) <|> (Prog r2) = Prog $ \t n ->
    let (actlist1, turtles1) = r1 t n
        (actlist2, turtles2) = r2 t n
    in (sortActions [actlist1, actlist2], turtles1 ++ turtles2)

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
  direction = (0, 1),
  pen       = Pen { penColor = Black, penDown = True }
}

-- # running functions
-- | the textual interface that prints what happens in sequential order
runTextual :: Program -> IO ()
runTextual (Prog p) =
  let (as, _) = p iniTurtle 0
  in printMsg as

printMsg :: [Action] -> IO ()
printMsg [] = return ()
printMsg (h:r) = do
    putStrLn . message . operation $ h
    printMsg r

-- | extrac the actions of a program for graphical rendering
runTurtle :: Program -> [Graph]
runTurtle (Prog p) =
  let (as, _)  = p iniTurtle 0
      graphs    = map (graph . operation) as
  in catMaybes graphs
