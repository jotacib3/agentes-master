module Board (initBoard,
             boardInsert,
             boardRemove,
             printBoard,
             populateBoard,
             coordinatesAreInBoard,
             getDirtNeighborCoordinates,
             getKidNeighborCoordinates,
             getPlaypenSpaceNeighborCoordinates,
             robotSpace,
            emptySpace,
            kidSpace,
            playpenSpace,
            dirtSpace) where

import Objects
import Data.Matrix

initBoard :: Int -> Int -> Matrix Object
initBoard sizeI sizeJ = matrix sizeI sizeJ (\(i, j) -> Empty)

-- boardInsert takes a Object, some coordinates, and the board and calls safeSet 
-- on the matrix to set the value of the cell to the object. boardInsert evaluates 
-- to a Maybe Matrix because the coordinates may be outside of the board, in which
-- case, safeSet evaluates to Nothing.
boardInsert :: Object -> Int -> Int -> Matrix Object -> Maybe (Matrix Object)
boardInsert object i j board = safeSet object (i, j) board


boardRemove :: Int -> Int -> Matrix Object -> Maybe (Matrix Object)
boardRemove i j board = safeSet Empty (i, j) board

-- Data.Matrix.prettyMatrix converts the matrix into a string
-- so it can be printed to the command line with putStrLn.
printBoard :: Matrix Object -> IO ()
printBoard board = putStrLn $ prettyMatrix board

-- verify if board [i,j] is empty
emptySpace :: Matrix Object -> (Int, Int) -> Bool
emptySpace board (i, j)
    = case safeGet i j board of
      Nothing -> False
      Just Empty -> True
      Just _ -> False

-- verify if board [i,j] is empty
dirtSpace :: Matrix Object -> (Int, Int) -> Bool
dirtSpace board (i, j)
    = case safeGet i j board of
      Nothing -> False
      Just Dirt -> True
      Just _ -> False

-- verify if board [i,j] is a home robot
robotSpace :: Matrix Object -> (Int, Int) -> Bool
robotSpace board (i, j)
    = case safeGet i j board of
      Nothing -> False
      Just (Robot _ _) -> True
      Just _ -> False

-- verify if board [i,j] is a kid on move
kidSpace :: Matrix Object -> (Int, Int) -> Bool
kidSpace board (i, j)
    = case safeGet i j board of
      Nothing -> False
      Just (Kid OnMove _) -> True
      Just _ -> False

-- verify if board [i,j] is a playpen
playpenSpace :: Matrix Object -> (Int, Int) -> Bool
playpenSpace board (i, j)
    = case safeGet i j board of
      Nothing -> False
      Just Playpen -> True
      Just _ -> False

populateBoard :: [(Object, Int, Int)] -> Matrix Object -> Maybe (Matrix Object)
populateBoard [] board = Just board
populateBoard ((object, i, j) : objects) board = boardInsert object i j board >>=
                                                   populateBoard objects

coordinatesAreInBoard :: (Int, Int) -> Matrix Object -> Bool
coordinatesAreInBoard (i, j) board
    = i >= 1 && i <= nrows board &&
      j >= 1 && j <= ncols board

-- TODO: refactor
getDirtNeighborCoordinates :: (Int, Int) -> Matrix Object -> [(Int, Int)]
getDirtNeighborCoordinates (x, y) board =
    [(x+a, y+b)
        | (a,b) <- [(0,-1),(0,1),(1,0),(-1,0), (1,1), (-1,-1), (1,-1), (-1,1)]
            , coordinatesAreInBoard (x+a, y+b) board &&  dirtSpace board (x+a, y+b)
        ]

getKidNeighborCoordinates :: (Int, Int) -> Matrix Object -> [(Int, Int)]
getKidNeighborCoordinates (x, y) board =
    [(x+a, y+b)
        | (a,b) <- [(0,-1),(0,1),(1,0),(-1,0), (1,1), (-1,-1), (1,-1), (-1,1)]
            , coordinatesAreInBoard (x+a, y+b) board && kidSpace board (x+a, y+b)
        ]

getEmptySpaceNeighborCoordinates :: (Int, Int) -> Matrix Object -> [(Int, Int)]
getEmptySpaceNeighborCoordinates (x, y) board =
    [(x+a, y+b)
        | (a,b) <- [(0,-1),(0,1),(1,0),(-1,0), (1,1), (-1,-1), (1,-1), (-1,1)]
            , coordinatesAreInBoard (x+a, y+b) board && emptySpace board (x+a, y+b)
        ]

getPlaypenSpaceNeighborCoordinates :: (Int, Int) -> Matrix Object -> [(Int, Int)]
getPlaypenSpaceNeighborCoordinates (x, y) board =
    [(x+a, y+b)
        | (a,b) <- [(0,-1),(0,1),(1,0),(-1,0), (1,1), (-1,-1), (1,-1), (-1,1)]
            , coordinatesAreInBoard (x+a, y+b) board && playpenSpace board (x+a, y+b)
        ]
