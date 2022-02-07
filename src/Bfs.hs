module Bfs (
    bfsToDirtOrKid,
    getNeighborDirtOrKid,
    getNeighborPlaypen,
    bfsToPlaypen,
    bfsToKid
    ) where

import Data.Matrix
import Control.Monad.State

import Objects
import Board
import Utils

-- get Neighbors kid, playpen, dirt and empty
getNeighborDirtOrKid :: Foldable t => Int -> Int -> t (Int, Int) -> Matrix Object -> [(Int, Int)]
getNeighborDirtOrKid x y visited board =
    filter (\(i, j) ->
        (i,j) `notElem` visited)
        [(x+a, y+b)
            | (a,b) <- [(0,-1),(0,1),(1,0),(-1,0), (1,1), (-1,-1), (1,-1), (-1,1)]
            , coordinatesAreInBoard (x+a, y+b) board
            && (
                kidSpace board (x+a, y+b)
                || playpenSpace board (x+a, y+b)
                || dirtSpace board (x+a, y+b)
                || emptySpace board (x+a, y+b)
            )
        ]

bfsToDirtOrKid :: [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), (Int, Int))] -> Matrix Object -> [(Int, Int)]
bfsToDirtOrKid [] _ _ _ = []
bfsToDirtOrKid ((i,j):xs) visited meta board
    -- if element is robot (initial position), empty or playpen then continue bfs 
    | emptySpace board (i,j) || robotSpace board (i,j) || playpenSpace board (i,j)
        = bfsToDirtOrKid (xs++neighbors) (visited++[(i,j)]) (meta++[((a,b), (i,j)) | (a,b)<-neighbors]) board
    
    -- if element is dirt or kid return the path
    | otherwise = constructPath (i,j) meta []
    where neighbors = getNeighborDirtOrKid i j visited board

------------------------------------------------------------------------
 
-- get Neighbors playpen, dirt and empty
getNeighborPlaypen :: Foldable t => Int -> Int -> t (Int, Int) -> Matrix Object -> [(Int, Int)]
getNeighborPlaypen x y visited board =
    filter (\(i, j) ->
        (i,j) `notElem` visited)
        [(x+a, y+b)
            | (a,b) <- [(0,-1),(0,1),(1,0),(-1,0), (1,1), (-1,-1), (1,-1), (-1,1)]
            , coordinatesAreInBoard (x+a, y+b) board
            && (
                dirtSpace board (x+a, y+b)
                || playpenSpace board (x+a, y+b)
                || emptySpace board (x+a, y+b)
            )
        ]
    
bfsToPlaypen :: [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), (Int, Int))] -> Matrix Object -> [(Int, Int)]
bfsToPlaypen [] _ _ _ = []
bfsToPlaypen ((i,j):xs) visited meta board
    -- if element is robot (initial position), empty or dirt then continue bfs 
    | not (playpenSpace board (i,j))
        = bfsToPlaypen (xs++neighbors) (visited++[(i,j)]) (meta++[((a,b), (i,j)) | (a,b)<-neighbors]) board
    
    -- if element is dirt or kid return the path
    | otherwise = constructPath (i,j) meta []
    where neighbors = getNeighborPlaypen i j visited board

--------------------------------------------------------------------------------
-- get Neighbors kid, playpen, dirt and empty
getNeighborKid :: Foldable t => Int -> Int -> t (Int, Int) -> Matrix Object -> [(Int, Int)]
getNeighborKid x y visited board =
    filter (\(i, j) ->
        (i,j) `notElem` visited)
        [(x+a, y+b)
            | (a,b) <- [(0,-1),(0,1),(1,0),(-1,0), (1,1), (-1,-1), (1,-1), (-1,1)]
            , coordinatesAreInBoard (x+a, y+b) board
            && (
                dirtSpace board (x+a, y+b)
                || kidSpace board (x+a, y+b)
                || emptySpace board (x+a, y+b)
            )
        ]
    
bfsToKid :: [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), (Int, Int))] -> Matrix Object -> [(Int, Int)]
bfsToKid [] _ _ _ = []
bfsToKid ((i,j):xs) visited meta board
    -- if element is robot (initial position), empty or dirt then continue bfs 
    | not (kidSpace board (i,j))
        = bfsToKid (xs++neighbors) (visited++[(i,j)]) (meta++[((a,b), (i,j)) | (a,b)<-neighbors]) board
    
    -- if element is dirt or kid return the path
    | otherwise = constructPath (i,j) meta []
    where neighbors = getNeighborKid i j visited board

---------------------------------------------------------------------------------------------------------

-- construct the path
constructPath :: (Num a, Num b, Eq a, Eq b) => (a, b) -> [((a, b), (a, b))] -> [(a, b)] -> [(a, b)]
constructPath (i,j) meta route
    | not (null points) = constructPath (head points) meta (route++[(i,j)])
    | otherwise = route++[(i,j)]
    where points = [(x,y) | ((a,b),(x,y)) <- meta, (a,b) == (i,j)]

-- bfsToDirtOrKid [start] [] [] matrix