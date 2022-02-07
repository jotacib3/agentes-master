module Environment (EnvironmentState(..),
              setIteration,
              getIteration,
              incrementIteration,
              setIO,
              getIO,
              setGenerator,
              getGenerator,
              setBoard,
              getBoard,
              makeEnvironment,
              performIO,
              Location(..),
              makeLocation) where

import Data.Matrix
import System.Random
import Control.Monad.State

import Objects
import Board

-- The EnvironmentState consists of an iteration count, an IO stream, a random number generator, and a board of Objects
data EnvironmentState g = EnvironmentState {iteration :: Int,
                                io :: IO (),
                                generator :: g,
                                nKids :: Int,
                                board :: Matrix Object}

setIteration :: RandomGen g => Int -> EnvironmentState g -> EnvironmentState g
setIteration newIteration EnvironmentState {iteration = _,
                                       io = thisIO,
                                       nKids = thisNKids,
                                       generator = thisGenerator,
                                       board = thisBoard}
    = EnvironmentState {iteration = newIteration,
                  io = thisIO,
                  nKids = thisNKids,
                  generator = thisGenerator,
                  board = thisBoard}

getIteration :: RandomGen g => EnvironmentState g -> Int
getIteration EnvironmentState {iteration = thisIteration} = thisIteration

incrementIteration :: RandomGen g => EnvironmentState g -> EnvironmentState g
incrementIteration EnvironmentState {iteration = thisIteration,
                                       io = thisIO,
                                       nKids = thisNKids,
                                       generator = thisGenerator,
                                       board = thisBoard}
    = EnvironmentState {iteration = thisIteration + 1,
                  io = thisIO,
                  nKids = thisNKids,
                  generator = thisGenerator,
                  board = thisBoard}


setIO :: RandomGen g => IO () -> EnvironmentState g -> EnvironmentState g
setIO newIO EnvironmentState {iteration = thisIteration,
                         io = _,
                         generator = thisGenerator,
                         nKids = thisNKids,
                         board = thisBoard}
    = EnvironmentState {iteration = thisIteration,
                  io = newIO,
                  generator = thisGenerator,
                  nKids = thisNKids,
                  board = thisBoard}

getIO :: RandomGen g => EnvironmentState g -> IO ()
getIO EnvironmentState {io = thisIO} = thisIO

setGenerator :: RandomGen g => g -> EnvironmentState g -> EnvironmentState g
setGenerator newGenerator EnvironmentState {iteration = thisIteration,
                                       io = thisIO,
                                       generator = _,
                                       nKids = thisNKids,
                                       board = thisBoard}
    = EnvironmentState {iteration = thisIteration,
                  io = thisIO,
                  nKids = thisNKids,
                  generator = newGenerator,
                  board = thisBoard}

getGenerator :: RandomGen g => EnvironmentState g -> g
getGenerator EnvironmentState {generator = thisGenerator} = thisGenerator

setBoard :: RandomGen g => Matrix Object -> EnvironmentState g -> EnvironmentState g
setBoard newBoard EnvironmentState {iteration = thisIteration,
                             io = thisIO,
                             generator = thisGenerator,
                             nKids = thisNKids,
                             board = _}
    = EnvironmentState {iteration = thisIteration,
                  io = thisIO,
                  generator = thisGenerator,
                  nKids = thisNKids,
                  board = newBoard}

getBoard :: RandomGen g => EnvironmentState g -> Matrix Object
getBoard EnvironmentState {board = thisBoard} = thisBoard

-- Create the EnvironmentState
makeEnvironment :: RandomGen g => Int -> IO () -> g -> Int -> Matrix Object -> Maybe (EnvironmentState g)
makeEnvironment thisIteration thisIO thisGenerator thisNKid thisBoard
    = Just (EnvironmentState {iteration = thisIteration,
                        io = thisIO,
                        generator = thisGenerator,
                        nKids = thisNKid,
                        board = thisBoard})

performIO :: RandomGen g => Maybe (EnvironmentState g) -> IO ()
performIO Nothing = return ()
performIO (Just EnvironmentState {io = thisIO}) = thisIO

data Location g = Location Object Int Int (EnvironmentState g)

makeLocation :: RandomGen g => Object -> EnvironmentState g -> (Int, Int) -> Location g
makeLocation object environmentState (i, j) = Location object i j environmentState

