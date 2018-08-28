module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { direction :: Bearing
                   , x         :: Integer
                   , y         :: Integer
                   } deriving (Show)

bearing :: Robot -> Bearing
bearing robot = direction robot

coordinates :: Robot -> (Integer, Integer)
coordinates robot = (x robot, y robot)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction' (x', y') = Robot {direction = direction', x = x', y = y'}

simulate :: Robot -> String -> Robot
simulate robot instructions = foldl f robot instructions
    where f r 'L' = r {direction = turnLeft $ bearing r}
          f r 'R' = r {direction = turnRight $ bearing r}
          f (Robot {direction = d, x = x', y = y'}) 'A' = case d of
                                                            North -> mkRobot North (x', y' + 1)
                                                            East -> mkRobot East (x' + 1, y')
                                                            South -> mkRobot South (x', y' - 1)
                                                            West -> mkRobot West (x' - 1, y')

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North
