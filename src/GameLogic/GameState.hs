{-# LANGUAGE OverloadedStrings #-}
module GameLogic.GameState where

import Prelude
import AiVsAi.UnitProperties

import Debug.Trace (trace)
import qualified Data.Set as Set
import Control.Monad.State
import Data.Map ((!), keys, elems, insert, Map, member)
import qualified Data.Map as Map 
--import qualified Data.Map as Map
--import Language.Haskell.Pretty 

import GHC.Generics

import AiVsAi.Util

import AiVsAi.GameData

import Data.Typeable.Internal (Typeable)
import Data.Data (Data)

import Data.List (partition, minimumBy)

import GameLogic.Deltas

import Data.Data (Data)
--import Data.Graph.AStar

import Control.Applicative ((<$>))

--import Data.Graph.Inductive.Graph
--import Data.Graph.Inductive.PatriciaTree



--handrolled pretty-printing class
class (Show a) => Pretty a where
    pretty :: a -> String
    pretty = show
    
    prettyList :: [a] -> String
    prettyList l = show $ map pretty l



--copy of error function, used in liquidHaskell to make sure never called
notImplemented = error




--Return an element if a predicate about it is true
maybeIf :: a -> (a -> Bool) -> Maybe a
maybeIf x f 
    |f x = Just x
    | otherwise =  Nothing

--Helper for checking if a tile is or isn't in range
--Always number 0 .. n-1
inRange ::  Int -> Int -> TileID -> Bool
inRange w h (x,y)
    | x < 0 = False
    | y < 0 = False
    | x >= w = False
    | y >= h = False
    | otherwise = True


instance Pretty Tile where
    pretty t = pretty $ occupant t

emptyTileInRange :: Int -> Int -> TileID ->  Tile
emptyTileInRange  w h (x, y) = Tile {
    occupant = Empty,
    xcoord = x,
    ycoord = y{-,
    left = maybeIf (x-1, y) (inRange w h),
    right = maybeIf (x+1, y) (inRange w h),
    top = maybeIf (x, y+1) (inRange w h),
    bottom = maybeIf (x-1, y-1) (inRange w h),
    topleft = maybeIf (x-1, y+1) (inRange w h),
    topright = maybeIf (x+1, y+1) (inRange w h),
    bottomleft = maybeIf (x-1, y-1) (inRange w h),
    bottomright = maybeIf (x+1, y-1) (inRange w h)-}
}

--Stores if this tile is map edge or not
--data AdjTile = AdjTile Tile | Edge



instance Pretty TileOccupant where
    pretty (TileUnit u) = pretty u
    pretty (TileEnv _) = "*"
    pretty Empty = "_"




--instance Pretty UnitID where
--    pretty = show

instance Pretty Int where
    pretty = show 
    


--A unit is it's ID number, it's type, and all of its state variables
--State is mutable, UnitType and ID are not
--type UnitStateM = State UnitState UnitID

--TODO why do we need newtype here?
--newtype Unit = Unit UnitState
--    deriving Show

    
showUnit :: UnitState -> String
showUnit _ = "Unit"



--TODO show health and stuff?
instance Pretty UnitState where
    pretty us = pretty $ unitType us

--Unit state monad



instance Pretty UnitType where
    pretty Scout = "S"
    pretty Tank = "T"    
    


    

{-
$(deriveJSON (drop 4) ''GameType)
$(deriveJSON (drop 4) ''EnvUnit)
$(deriveJSON (drop 4) ''TileOccupant)
$(deriveJSON (drop 4) ''Tile)
-- $(deriveJSON (drop 4) ''Team)
-- $(deriveJSON (drop 4) ''UnitType)
$(deriveJSON (drop 4) ''UnitState)
$(deriveJSON (drop 4) ''Map.Map)
$(deriveJSON (drop 4) ''GameState)
-}

--Initialize the game with completely empty squares
initialState :: Int -> Int -> GameType -> GameState
initialState w h gt = GameState{
    gameTiles = emptyTiles,
    gameUnits = Map.empty,
    turn = 0,
    gameMapWidth = w,
    gameMapHeight = h,
    gameType = gt,
    unitQueue = []
    }
    where 
    tileIDs = [(x,y) | x <- [1 .. w], y <- [1 .. h] ]
    tileContents = [(square, emptyTile square ) | square <- tileIDs ]
    emptyTile = emptyTileInRange w h
    emptyTiles = Map.fromList tileContents

--handy map printing function
instance Pretty GameState where
    pretty = showMap
    
showMap :: GameState -> String
showMap gs = mapString
    where
    tileMap = gameTiles gs
    rowString r = concat  [pretty (tileMap Map.! (i,r)) ++ " "| i <- [1.. gameMapWidth gs] ]
    mapString = concat [ "|" ++ rowString r ++ "|\n" | r <- [1 .. gameMapHeight gs]]
    
--statePlusOccupant :: TileOccupant -> TileID -> GameState -> GameState






--Infinite list of game states after a given state
--TODO add other input parameters    
--TODO get rid of dummy number vars    
--gameRun ::  [GameState]
--gameRun = statesAfter $ initialState 5 5




setDuration :: GameType -> Int -> GameType
setDuration (Endurance _) dur = Endurance dur
setDuration gt _ = gt 

result :: GameState -> Maybe Result
result gs = endForType (gameType gs) gs


--Ending scenario for each game type        
endForType :: GameType -> GameState -> Maybe Result 
endForType Deathmatch gs 
    | null $ Map.elems (gameUnits gs) = Just Draw 
    | null $ filter (\u ->  unitTeam u == TeamC) $ Map.elems $ gameUnits gs  = Just FWin
    | null $ filter (\u ->  unitTeam u == TeamF) $ Map.elems $ gameUnits gs  = Just CWin
    | otherwise = Nothing
    
        

endForType (Endurance maxMoves) gs
    | turn gs < maxMoves = Nothing
    | numF > numC = Just FWin
    | numF < numC = Just CWin
    | otherwise = Just Draw
    where
        numF = length $ filter (\u ->  unitTeam u == TeamF) $Map.elems $ gameUnits gs
        numC = length $ filter (\u ->  unitTeam u == TeamC) $Map.elems $ gameUnits gs
        
gsid :: GameState -> GameState
gsid = id

unitQueues :: GameState -> ([UnitID], [UnitID])
unitQueues gs = partition (\uid -> (unitTeam ((gameUnits gs) ! uid)) == TeamC ) $ keys $ gameUnits gs




--type MapGraph = Gr TileID (TileID, TileID)



{-
--There are incoming edges to full tiles
--But no outgoing edges
unitGraph :: TileID -> GameState -> TileID -> Set.Set TileID        
unitGraph start  gs vertex 
    |(occupantAt vertex gs) == Empty =  Set.fromList emptyTilesAdjacent
    |otherwise = Set.fromList []
    where
        fand f g x= (f x) && (g x) 
        allNeighbours (x,y) = [(x+1,y), (x-1,y), (x+1,y-1), (x,y-1), (x-1,y-1), (x+1,y+1), (x,y+1), (x-1,y+1) ]
        --tileEmpty t = (occupantAt t gs) == Empty
        tileInRange (x,y) = x >= 0 && x <= (gameMapWidth gs) && y <= (gameMapHeight gs) && y >= 0
        emptyTilesAdjacent = filter  tileInRange $ allNeighbours vertex
-}


--TODO move these to util? or new module?
--TODO need to account for 1-way direction for euclidDist?


--easy getters and setters



unitAlive gs uid = (member  uid (gameUnits gs)) && (getUnitHealth gs uid) > 0

validGameUnits :: GameState -> Bool
validGameUnits gs = (and $ map validPos (keys $ gameUnits gs))
    where
        oat gs tid = occupantAt tid gs
        validPos uid = (oat gs $ getUnitPos gs uid) == TileUnit uid

            
validGameTiles :: GameState -> Bool
validGameTiles gs = (and $ map validTile (keys $ gameTiles gs)) 
    where
        validTile tid = case (occupantAt tid gs) of
            TileUnit u -> (getUnitPos gs u) == tid
            _ -> True
            
