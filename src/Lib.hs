module Lib (runSimulation) where

import System.IO
import Data.List
import Data.Matrix
import Control.Monad.State
import System.Random
import System.Random.Shuffle

import Objects
import Board
import Environment
import Behaviors
import Random

-- simulateEnvironment updates the state of the world
simulateEnvironment :: RandomGen g => EnvironmentState g -> Maybe (EnvironmentState g)
simulateEnvironment worldState = Just (updateEnvironment worldState)

-- printEnvironment adds a printBoard call to the IO stream of the EnvironmentState
printEnvironment :: RandomGen g => EnvironmentState g -> Maybe (EnvironmentState g)
printEnvironment worldState@(EnvironmentState {io = thisIO, board = thisBoard})
    = Just (setIO (thisIO >> printBoard thisBoard) worldState)

-- waitEnvironment adds some print-outs and a getChar to the IO stream of the EnvironmentState
waitEnvironment :: RandomGen g => EnvironmentState g -> Maybe (EnvironmentState g)
waitEnvironment worldState@(EnvironmentState {io = thisIO})
    = Just (setIO (thisIO >> putStrLn "-----" >> hFlush stdout >> getChar >> return ()) worldState)

-- mueve las posiciones de los niños y el robot si la iteración actual es congruente 0 módulo 4
randomizeEnvironment :: RandomGen g => EnvironmentState g -> Maybe (EnvironmentState g)
randomizeEnvironment worldState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator, iteration = thisIteration}
    | thisIteration `rem` 4 == 0 = Just newEnvironment
    | otherwise = Just worldState
        where dirts_count = 4
              rows = nrows thisBoard
              columns = ncols thisBoard
              cells = shuffle' ((,) <$> [1..rows] <*> [1..columns]) (rows * columns) thisGenerator

              -- find robot, empty and kid positions
              (robotI, robotJ) = head (filter (\(i, j) -> robotSpace thisBoard (i, j)) cells)
              kidCells = filter (\(i, j) -> kidSpace thisBoard (i, j)) cells
              emptyCells = filter (\(i, j) -> emptySpace thisBoard (i, j)) cells

              -- take new positions
              kidCellsCount = length kidCells
              newKidsPositions = take kidCellsCount emptyCells
              (newRobotI, newRobotJ) = emptyCells !! (kidCellsCount + 1)

              newEnvironmentWithRobot = unsafeSetObject (Robot OnMove False) newRobotI newRobotJ (unsafeSetObject Empty robotI robotJ worldState)
              newEnvironment = foldr setToEmpty (foldr setToKid newEnvironmentWithRobot newKidsPositions) kidCells

-- pinta el estado actual de vacías y sucias en cada iteración
printStatics :: RandomGen g => EnvironmentState g -> Maybe (EnvironmentState g)
printStatics worldState@EnvironmentState {io = thisIO, board = thisBoard, generator = thisGenerator}
    = let dirts_count = 4
          rows = nrows thisBoard
          columns = ncols thisBoard
          cells = shuffle' ((,) <$> [1..rows] <*> [1..columns]) (rows * columns) thisGenerator
          freeCells = filter (\(i, j) -> emptySpace thisBoard (i, j)) cells
          dirtCells = filter (\(i, j) -> dirtSpace thisBoard (i, j)) cells
          freeCellsCount = length freeCells
          dirtCellsCount = length dirtCells
          total = freeCellsCount + dirtCellsCount
    in Just (setIO (thisIO >>
        putStrLn "Dirt and Empty cells relations: " >>
        putStrLn ("Empty cells count: " ++ show freeCellsCount) >>
        putStrLn ("Dirts cells count: " ++ show dirtCellsCount) >>
        putStrLn ("% Empty cell: " ++ show (freeCellsCount*100 `div` total))) worldState)

-- incrementEnvironment increments the current iteration of the EnvironmentState:
incrementEnvironment :: RandomGen g => EnvironmentState g -> Maybe (EnvironmentState g)
incrementEnvironment worldState = Just (incrementIteration worldState)

maybeStep :: RandomGen g => (EnvironmentState g -> (Maybe (EnvironmentState g))) -> State (Maybe (EnvironmentState g)) (Maybe (EnvironmentState g))
maybeStep updateFunction = state (\worldState -> let newEnvironmentState = worldState >>= updateFunction --worldState has type Mabye (EnvironmentState g)
                                                 in (newEnvironmentState, newEnvironmentState))

-- simulate state machine
simulate :: RandomGen g => State (Maybe (EnvironmentState g)) (Maybe (EnvironmentState g))
simulate = maybeStep simulateEnvironment >>
           maybeStep randomizeEnvironment >>
           maybeStep printEnvironment >>
           maybeStep printStatics >>
        --    maybeStep waitEnvironment >>      -- descomentar para correr proceso iteracion por iteracion en 
           maybeStep incrementEnvironment >>=
           (\worldState ->
             case worldState of
             Nothing -> get
             Just (EnvironmentState {iteration = thisIteration})
                -> if thisIteration > 14    -- time
                   then get
                   else simulate)

-- runSimulation creates an initialBoard with the initBoard function. 
-- It then creates a standard random number generator with the mkStdGen 
-- function and a random seed value.
runSimulation :: IO ()
runSimulation = let rows = 10
                    columns = 10

                    -- generate random count for objects you can use function rand min max
                    dirts_count = 30
                    obstacles_count = rand 3 6
                    kids_count = 1

                    -- creates an initialBoard with the initBoard function
                    initialBoard = initBoard rows columns

                    -- create a standard random number generator with the mkStdGen function and a random seed value
                    generator = mkStdGen 126590563

                    -- generate a list of coordinates for the initial population 
                    -- The result of this sub-expression is a list of all combinations of elements in [1..rows] joined 
                    -- with all elements of [1..columns] joined by the tuple operator (,). The tuple operator creates a 
                    -- tuple out of its parameters. The result of this computation is a list of all unique coordinate 
                    -- tuples in the board.
                    -- shuffle’ takes a list, the size of the list, and a random number generator. It evaluates to 
                    -- a randomly shuffled version of the list
                    coordinates = filter playpenFilter (shuffle' ((,) <$> [1..rows] <*> [1..columns]) (rows * columns) generator)

                    -- playpen coordinates in the board
                    playpenCoordinates = [(2,2),(3,3),(3,2),(2,3)]

                    -- initial dirts coordinates in the board
                    dirtsCoordinates = take dirts_count coordinates
                    newCoordinates = drop dirts_count coordinates

                    -- obstacles coordinates in the board
                    obstaclesCoordinates = take obstacles_count newCoordinates
                    newCoordinates1 = drop obstacles_count newCoordinates

                    -- kids coordinates in the board
                    kidsCoordinates = take kids_count newCoordinates1
                    newCoordinates2 = drop kids_count newCoordinates1

                    -- robots coordinates in the board
                    robotsCoordinates = take 1 newCoordinates2

                    -- generate object population
                    kidObjectsPositions = unfoldr (generatePopulation (Kid OnMove False) ) (kidsCoordinates, generator)
                    robotsObjectsPositions = unfoldr (generatePopulation (Robot OnMove False) ) (robotsCoordinates, generator)
                    playpenObjectsPositions = unfoldr (generatePopulation Playpen ) (playpenCoordinates, generator)
                    dirtObjectsPositions = unfoldr (generatePopulation Dirt) (dirtsCoordinates, generator)
                    obstacleObjectPositions = unfoldr (generatePopulation Obstacle) (obstaclesCoordinates, generator)

                    -- objects positions contain all object and respective board position
                    objectPositions = kidObjectsPositions
                        ++ robotsObjectsPositions
                        ++ playpenObjectsPositions
                        ++ dirtObjectsPositions
                        ++ obstacleObjectPositions

                    -- populate the board
                    iBoard = populateBoard objectPositions initialBoard

                -- print Initial state
                in putStrLn ("Starting Simulation.\n" ++
                    "The number initial of kids is " ++ show kids_count ++ "\n"++
                    "The number initial of dirts is " ++ show dirts_count ++ "\n" ++
                    "The number initial of obstacles is " ++ show obstacles_count ++ "\n"
                ) >>
                    putStr "Playpen coordinates: " >>
                    print playpenCoordinates >>
                    putStr "Kids coordinates: " >>
                    print kidsCoordinates >>
                    putStr "Robots coordinates: " >>
                    print robotsCoordinates >>
                    putStr "Dirts coordinates: " >>
                    print dirtsCoordinates >>
                    putStr "Obstacles coordinates: " >>
                    print obstaclesCoordinates >>
                    putStrLn "LOADING ... " >>
                    -- bind the result of the populateBoard function (iBoard) which is a Maybe (Matrix Object), to the makeEnvironment function.
                    -- The result is a Maybe (Matrix Object), which is used as the initial state of the simulation.
                    -- I call evalState on the simulate state processor, which performs the simulation on the world. Then I call 
                    -- performIO on the Maybe (EnvironmentState g) to print the state of the world to standard output
                   performIO (evalState simulate (iBoard >>= makeEnvironment 1 (return ()) generator kids_count))
