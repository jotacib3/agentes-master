module Behaviors (updateObject,
                  setToEmpty,
                  unsafeSetObject,
                  setToKid,
                  updateEnvironment) where

import Data.List
import Data.Matrix
import System.Random
import Control.Monad.State

import Objects
import Board
import Environment
import Random
import Bfs
import Utils


unsafeSetObject :: RandomGen g => Object -> Int -> Int -> EnvironmentState g -> EnvironmentState g
unsafeSetObject object i j environmentState@(EnvironmentState {board = thisBoard})
    = setBoard (unsafeSet object (i, j) thisBoard) environmentState

unsafeSetObjectInLocation :: RandomGen g => Object -> Int -> Int -> Location g -> Location g
unsafeSetObjectInLocation object i j (Location _ _ _ environmentState)
    = Location object i j (unsafeSetObject object i j environmentState)

-- run bfs looking for the nearest dirt or kid in the board and return the next position for move robot
betterPositionToReachKidOrGarbage :: (Int, Int) -> Matrix Object -> ((Int, Int), [(Int,Int)])
betterPositionToReachKidOrGarbage initialCoord board =
  let path = bfsToDirtOrKid [initialCoord] [] [] board
  in (nextPosition path initialCoord, path)

-- run bfs looking for the nearest dirt or kid in the board and return the next position for move robot
betterPositionToReachPlaypen :: (Int, Int) -> Matrix Object -> ((Int, Int), [(Int,Int)])
betterPositionToReachPlaypen initialCoord board =
  let path = bfsToPlaypen [initialCoord] [] [] board
  in (nextPosition path initialCoord, path)

-- run bfs looking for the nearest dirt or kid in the board and return the next position for move robot
betterPositionToReachKid :: (Int, Int) -> Matrix Object -> ((Int, Int), [(Int,Int)])
betterPositionToReachKid initialCoord board =
  let path = bfsToKid [initialCoord] [] [] board
  in (nextPosition path initialCoord, path)

cleanCell :: RandomGen g => [(Int,Int)] -> Location g -> (Location g, Location g)
cleanCell dirts location@(Location object i j environmentState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator})
        = let (newI, newJ) = head dirts
              elementInNextPosition = unsafeGet newI newJ thisBoard
              (leaveObject, currentObjectUpdated) = leavingObject object elementInNextPosition

              -- set acted to true
              newObject = setObjectActed True currentObjectUpdated

              -- update board with movement
              newBoard = unsafeSet leaveObject (i, j) (unsafeSet newObject (newI, newJ) thisBoard)

              -- logger
              newEnvironmentState = setIO (thisIO >>
                putStr (show i ++ ", " ++ show j ++ " (" ++ show object ++ "): ") >>
                putStrLn ("moves to cell "++ " (" ++ show newI ++ ", " ++ show newJ ++ ") " ++ " and cleans it" )) environmentState

              newLocation = Location newObject newI newJ (setBoard newBoard newEnvironmentState)
        in (newLocation, newLocation)

pickUpKid :: RandomGen g => [(Int,Int)] -> Location g -> (Location g, Location g)
pickUpKid kids location@(Location object i j environmentState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator})
        = let
              -- get the first kid
              (newI, newJ) = head kids

              -- set robot acted to true and state to WithKid
              newObject = setObjectActed True (setState WithKid object)

              -- clean robot cell and move to kid cell
              newBoard = unsafeSet Empty (i, j) (unsafeSet newObject (newI, newJ) thisBoard)

              -- logger
              newEnvironmentState = setIO (thisIO >>
                putStr (show i ++ ", " ++ show j ++ " (" ++ show object ++ "): ") >>
                putStrLn ("moves to cell "++ " (" ++ show newI ++ ", " ++ show newJ ++ ") " ++ " and pick up the kid" )) environmentState

              -- update location
              newLocation = Location object newI newJ (setBoard newBoard newEnvironmentState)

        in (newLocation, newLocation)

leaveKid :: RandomGen g => [(Int,Int)] -> Location g -> (Location g, Location g)
leaveKid playpens location@(Location object i j environmentState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator})
        = let
              -- get the first kid
              (newI, newJ) = head playpens

              -- set robot acted to true and state to WithKidPlaypen
              newObject = setObjectActed True (setState WithKidPlaypen object)

              -- clean robot cell and move to kid cell
              newBoard = unsafeSet Empty (i, j) (unsafeSet newObject (newI, newJ) thisBoard)

              newEnvironmentState = setIO (thisIO >>
                putStr (show i ++ ", " ++ show j ++ " (" ++ show object ++ "): ") >>
                putStrLn ("moves to playpen cell "++ " (" ++ show newI ++ ", " ++ show newJ ++ ") " ++ " for leave the kid" )) environmentState

              -- update location
              newLocation = Location object newI newJ (setBoard newBoard newEnvironmentState)

        in (newLocation, newLocation)

-- find the best position to move, then move and leave leaveObject in initial cell
moveToBest :: RandomGen g =>Location g -> (Location g, Location g)
moveToBest location@(Location object i j environmentState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator})
    = let
        -- run bfs to find best position to mov
        result = betterPositionToReachKidOrGarbage (i,j) thisBoard

        -- get min path
        path =  reverse (snd result)

        -- get next position to go
        (bestI, bestJ) = fst result

        elementInNextPosition = unsafeGet bestI bestJ thisBoard

        (leaveObject, currentObjectUpdated)= leavingObject object elementInNextPosition

        -- set robot acted to true
        newObject = setObjectActed True currentObjectUpdated

        -- update board

        newBoard = unsafeSet leaveObject (i, j) (unsafeSet newObject (bestI, bestJ) thisBoard)

        -- print action
        newEnvironmentState = setIO (thisIO >>
          putStr (show i ++ ", " ++ show j ++ " (" ++ show object ++ "): ") >>
          putStr ("moves to cell "++ " (" ++ show bestI ++ ", " ++ show bestJ ++ ") " ++ " as best movement(greedy) and path ") >>
          print path
          ) environmentState

        newLocation = Location newObject i j (setBoard newBoard newEnvironmentState)
  in (newLocation, newLocation)

moveToKid :: RandomGen g => Location g -> (Location g, Location g)
moveToKid location@(Location object i j environmentState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator})
    = let
        -- run bfs to find best position to mov
        result = betterPositionToReachKid (i,j) thisBoard

        -- get min path
        path =  reverse (snd result)

        -- get next position to go
        (bestI, bestJ) = fst result

        -- set robot acted to true
        newObject = setObjectActed True object

        boardWithRobotUpdated = unsafeSet newObject (bestI, bestJ) thisBoard

        -- update board
        newBoard = updatedToEmpty (i,j) (bestI,bestJ) boardWithRobotUpdated

        -- print action
        newEnvironmentState = setIO (thisIO >>
          putStr (show i ++ ", " ++ show j ++ " (" ++ show object ++ "): ") >>
          putStr ("moves to cell "++ " (" ++ show bestI ++ ", " ++ show bestJ ++ ") " ++ show newObject ++ " finding playpen with path ") >>
          print path) environmentState


        newLocation = Location newObject bestI bestJ (setBoard newBoard newEnvironmentState)
  in (newLocation, newLocation)

moveToPlaypen :: RandomGen g => Location g -> (Location g, Location g)
moveToPlaypen location@(Location object i j environmentState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator})
    = let
        -- run bfs to find best position to mov
        result = betterPositionToReachPlaypen (i,j) thisBoard

        -- get min path
        path =  reverse (snd result)

        -- get next position to go
        (bestI, bestJ) = fst result

        -- set robot acted to true
        newObject = setObjectActed True object

        boardWithRobotUpdated = unsafeSet newObject (bestI, bestJ) thisBoard

        -- update board
        newBoard = updatedToEmpty (i,j) (bestI,bestJ) boardWithRobotUpdated

        -- print action
        newEnvironmentState = setIO (thisIO >>
          putStr (show i ++ ", " ++ show j ++ " (" ++ show object ++ "): ") >>
          putStr ("moves to cell "++ " (" ++ show bestI ++ ", " ++ show bestJ ++ ") " ++ show newObject ++ " finding playpen with path ") >>
          print path) environmentState


        newLocation = Location newObject bestI bestJ (setBoard newBoard newEnvironmentState)
  in (newLocation, newLocation)

-- robot state machine
moveRobotAction :: RandomGen g => Location g -> (Location g, Location g)
moveRobotAction location@(Location object i j environmentState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator})
      | not (null dirts)                                          -- clean cell
          = cleanCell dirts location
      | not (null kids) && getState object == Just OnMove       -- pick up kid
          = pickUpKid kids location
      | not (null playpens) && getState object == Just WithKid  -- move to playpen in neighbor
          = leaveKid playpens location
      | null playpens && getState object == Just WithKid        -- move finding playpen
          = moveToPlaypen location
      | otherwise = moveToBest location         
      where playpens = getPlaypenSpaceNeighborCoordinates (i,j) thisBoard
            dirts = getDirtNeighborCoordinates (i,j) thisBoard
            kids = getKidNeighborCoordinates (i,j) thisBoard

-- robot state machine
moveRobotActionKid :: RandomGen g => Location g -> (Location g, Location g)
moveRobotActionKid location@(Location object i j environmentState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator})
      | not (null dirts)                                          -- clean cell
          = cleanCell dirts location
      | not (null kids) && getState object == Just OnMove       -- pick up kid
          = pickUpKid kids location
      | not (null playpens) && getState object == Just WithKid  -- move to playpen in neighbor
          = leaveKid playpens location
      | null playpens && getState object == Just WithKid        -- move finding playpen
          = moveToPlaypen location
      | otherwise = moveToKid location         
      where playpens = getPlaypenSpaceNeighborCoordinates (i,j) thisBoard
            dirts = getDirtNeighborCoordinates (i,j) thisBoard
            kids = getKidNeighborCoordinates (i,j) thisBoard

-- Move the kid to new board position
moveKid :: RandomGen g => (Int, Int) -> Location g -> Object -> (Location g, Location g)
moveKid (newI, newJ) location@(Location object i j environmentState@(EnvironmentState {io = thisIO, board = thisBoard})) dirtObject
    | coordinatesAreInBoard (newI, newJ) thisBoard &&
      unsafeGet newI newJ thisBoard == Empty
        = let newBoard = unsafeSet dirtObject (i, j) (unsafeSet object (newI, newJ) thisBoard)
              newEnvironmentState = setIO (thisIO >>
                putStr (show i ++ ", " ++ show j ++ " (" ++ show object ++ "): ") >>
                putStr ("moves to cell "++ " (" ++ show newI ++ ", " ++ show newJ ++ ") and leave " ++ show dirtObject ++ " \n")) environmentState
              newLocation = Location object newI newJ (setBoard newBoard newEnvironmentState)
          in (newLocation, newLocation)
    | coordinatesAreInBoard (newI, newJ) thisBoard &&
      unsafeGet newI newJ thisBoard == Playpen
        = let newBoard = unsafeSet dirtObject (i, j) (unsafeSet newObject (newI, newJ) thisBoard)
              newEnvironmentState = setIO (thisIO >>
                putStr (show i ++ ", " ++ show j ++ " (" ++ show object ++ "): ") >>
                putStr ("moves to cell "++ " (" ++ show newI ++ ", " ++ show newJ ++ ") and leave " ++ show dirtObject ++ " \n")) environmentState
              newLocation = Location object newI newJ (setBoard newBoard newEnvironmentState)
          in (newLocation, newLocation)
    | otherwise = (location, location)
        where
            newObject = setState OnPlaypen object

move :: RandomGen g => Location g -> (Location g, Location g)
move location@(Location object i j environmentState@(EnvironmentState {generator = thisGenerator}))
    | randomNumber == 0 = moveKid (i - 1, j) (Location newObject i j newEnvironmentState) dirtObject
    | randomNumber == 1 = moveKid (i + 1, j) (Location newObject i j newEnvironmentState) dirtObject
    | randomNumber == 2 = moveKid (i, j - 1) (Location newObject i j newEnvironmentState) dirtObject
    | randomNumber == 3 = moveKid (i, j + 1) (Location newObject i j newEnvironmentState) dirtObject
    | randomNumber == 4 = moveKid (i - 1, j - 1) (Location newObject i j newEnvironmentState) dirtObject
    | randomNumber == 5 = moveKid (i + 1, j + 1) (Location newObject i j newEnvironmentState) dirtObject
    | randomNumber == 6 = moveKid (i - 1, j + 1) (Location newObject i j newEnvironmentState) dirtObject
    | randomNumber == 7 = moveKid (i + 1, j - 1) (Location newObject i j newEnvironmentState) dirtObject
    | otherwise = let newLocation = Location newObject i j newEnvironmentState in (newLocation, newLocation)
        where (randomNumber, newGenerator) = randomR (0 :: Int, 7 ::Int) thisGenerator
              newObject = setObjectActed True object
              newEnvironmentState = unsafeSetObject newObject i j $ setGenerator newGenerator environmentState
              dirtObject = if randomNumber < 4 then Dirt else Empty -- probability of 50% for dirt

-- performBehavior is responsible for run action updating the EnvironmentState based on the Object‘s current state.
performBehavior :: RandomGen g => Location g -> (Location g, Location g)
performBehavior location@(Location Empty _ _ _) = (location, location)
performBehavior location@(Location Dirt _ _ _) = (location, location)
performBehavior location@(Location Obstacle  _ _ _) = (location, location)
performBehavior location@(Location Playpen  _ _ _) = (location, location)
performBehavior location@(Location (Kid OnPlaypen  _)  _ _ _) = (location, location)
performBehavior location@(Location (Kid OnMove  _)  _ _ _) = move location
performBehavior location@(Location (Robot _ _)  _ _ _) = moveRobotAction location
performBehavior location = (location, location)

-- Each Object has a Bool which stores whether it has acted on this iteration.
-- resetObject sets the active Bool to False for each object so they can act again next iteration
setToEmpty :: RandomGen g => (Int, Int) -> EnvironmentState g -> EnvironmentState g
setToEmpty (i, j) environmentState@EnvironmentState {board = thisBoard}
     = unsafeSetObject Empty i j environmentState

setToKid :: RandomGen g => (Int, Int) -> EnvironmentState g -> EnvironmentState g
setToKid (i, j) environmentState@EnvironmentState {board = thisBoard}
     = unsafeSetObject (Kid OnMove False) i j environmentState
    
-- updateObject is responsible for performing all of the behaviors for each object in the environment
updateObject :: RandomGen g => (Int, Int) -> EnvironmentState g -> EnvironmentState g
updateObject (i, j) environmentState@(EnvironmentState {board = thisBoard})
    | coordinatesAreInBoard (i, j) thisBoard &&
      object /= Empty &&
      object /= Dirt  &&
      object /= Obstacle &&
      object /= Playpen  &&
      not (objectHasActed object)
          = let (Location _ _ _ newEnvironmentState)
                    = execState (state performBehavior) (Location object i j environmentState)
            in newEnvironmentState
    | otherwise = environmentState
        where object = unsafeGet i j thisBoard

-- Each Object has a Bool which stores whether it has acted on this iteration.
-- resetObject sets the active Bool to False for each object so they can act again next iteration
resetObject :: RandomGen g => (Int, Int) -> EnvironmentState g -> EnvironmentState g
resetObject (i, j) environmentState@(EnvironmentState {board = thisBoard})
    | coordinatesAreInBoard (i, j) thisBoard &&
      object /= Empty
          = unsafeSetObject (setObjectActed False object) i j environmentState
    | otherwise = environmentState
        where object = unsafeGet i j thisBoard

-- updateEnvironment takes the existing EnvironmentState and calls updateObject at every coordinate
-- with foldr, followed by calling resetObject on every coordinate
updateEnvironment :: RandomGen g => EnvironmentState g -> EnvironmentState g
updateEnvironment environmentState@EnvironmentState {iteration = thisIteration,
                                    io = thisIO,
                                    board = thisBoard}
    = foldr resetObject (foldr updateObject newEnvironmentState coordinates) coordinates
        where newEnvironmentState = setIO (thisIO >> putStrLn ("\n Time: " ++ show thisIteration ++ " unities" )) environmentState
              coordinates = (,) <$> [1 .. (nrows thisBoard)] <*> [1 .. (ncols thisBoard)]

