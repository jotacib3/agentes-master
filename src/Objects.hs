module Objects (ObjectState(..),
                  Object(..),
                  getState,
                  setState,
                  objectHasActed,
                  setObjectActed,
                  leavingObject,
                  generatePopulation,
                  updatedToEmpty,
                  playpenFilter) where

import System.Random
import Data.Matrix

data ObjectState = WithKidPlaypen | WithKid | OnMove | OnPlaypen deriving (Show, Eq, Ord)

showSmall :: ObjectState -> String
showSmall WithKidPlaypen = "KP"
showSmall WithKid = "K"
showSmall OnMove = "M"
showSmall OnPlaypen = "P"

data Object = Empty |
                Dirt |
                Obstacle |
                Playpen |
                Kid ObjectState Bool |
                Robot ObjectState Bool
                deriving (Eq, Ord)

instance Show Object where
    show Empty = "[ ]"
    show Dirt = "[*]"
    show Playpen = "[P]"
    show Obstacle = "[X]"
    show (Kid s _) = "[K" ++ showSmall s ++ "]"
    show (Robot s a)= "[R" ++ showSmall s ++ "]"

getState :: Object -> Maybe ObjectState
getState Empty = Nothing
getState Dirt = Nothing
getState Playpen = Nothing
getState Obstacle = Nothing
getState (Kid s _) = Just s
getState (Robot s _) = Just s

setState :: ObjectState -> Object -> Object
setState s Empty = Empty
setState s Dirt = Dirt
setState s Playpen = Playpen
setState s Obstacle = Obstacle
setState s (Kid _ a) = Kid s a
setState s (Robot _ a) = Robot s a

-- Object has acted
objectHasActed :: Object -> Bool
objectHasActed Empty = False
objectHasActed Dirt = False
objectHasActed Playpen = False
objectHasActed Obstacle = False
objectHasActed (Kid _ a) = a
objectHasActed (Robot _ a) = a

setObjectActed :: Bool -> Object -> Object
setObjectActed _ Empty = Empty
setObjectActed _ Dirt = Dirt
setObjectActed _ Playpen = Playpen
setObjectActed _ Obstacle = Obstacle
setObjectActed a (Kid s _) = Kid s a
setObjectActed a (Robot s _) = Robot s a

updatedToEmpty :: (Int, Int) -> (Int, Int) -> Matrix Object -> Matrix Object
updatedToEmpty coord1 coord2 board
    | coord1 /= coord2 = unsafeSet Empty coord1 board
    | otherwise = board

leavingObject :: Object -> Object -> (Object, Object)
leavingObject (Robot WithKidPlaypen _) Playpen = (Kid OnPlaypen False, Robot OnPlaypen False) 
leavingObject (Robot WithKidPlaypen _) Empty = (Kid OnPlaypen False, Robot OnMove False) 
leavingObject (Robot WithKidPlaypen _) Dirt = (Kid OnPlaypen False, Robot OnMove False) 
leavingObject (Robot OnPlaypen _) Playpen = (Playpen, Robot OnPlaypen False)
leavingObject (Robot OnPlaypen _) Empty = (Playpen, Robot OnMove False)
leavingObject (Robot OnPlaypen _) Dirt = (Playpen, Robot OnMove False)
leavingObject _ _= (Empty, Robot OnMove False)

playpenFilter :: (Int, Int) -> Bool
playpenFilter (a,b) = not (elem a [2, 3] && elem b [2, 3])

generatePopulation :: RandomGen g => Object -> ([(Int, Int)], g) -> Maybe ((Object, Int, Int), ([(Int, Int)], g))
generatePopulation object ([], generator) = Nothing
generatePopulation object ((i, j) : coords, generator) = Just ((object, i, j), (coords, generator))


