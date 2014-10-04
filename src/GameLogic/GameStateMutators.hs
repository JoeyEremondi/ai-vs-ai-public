module GameLogic.GameStateMutators where

import Prelude
import AiVsAi.GameData
import GameLogic.GameState

import Data.Map ((!))
import qualified Data.Map as Map
import AiVsAi.UnitProperties
import AiVsAi.Util

import Control.Exception (assert)

        
statePlusTree :: TileID -> GameState -> GameState    
statePlusTree treeLoc gs =
    gs {gameTiles = newTiles}
    where 
        oldTiles = gameTiles gs
        newTiles = Map.insert treeLoc treeTile oldTiles 
        treeTile = (oldTiles Map.! treeLoc) {occupant = TileEnv Tree}

--Similar to StatePlusTree, but we need to add unit to list of units
statePlusUnit :: UnitID -> Team -> UnitType -> TileID -> GameState -> GameState
statePlusUnit uid team theUnitType unitLoc gs = 
    gs { 
    gameTiles  = newTiles, 
    gameUnits = newUnits }
    where 
        oldTiles = gameTiles  gs
        oldUnits = gameUnits  gs
        newTiles = Map.insert unitLoc unitTile oldTiles 
        newUnits = Map.insert uid unitState oldUnits
        unitTile = (oldTiles Map.! unitLoc) {occupant = TileUnit uid}
        unitState =   newUnit uid unitLoc theUnitType team
        
moveUnit :: UnitID -> TileID -> GameState -> GameState
moveUnit uid newLoc gs =
    --Precond
    assert (occupantAt newLoc gs == Empty)
    --Postcond
    assert (getUnitPos newGState uid == newLoc) $ 
    assert (occupantAt (getUnitPos gs uid) newGState == Empty) $
    assert (validGameUnits newGState) $
    assert (validGameTiles newGState) $
    newGState
    where 
        newGState = gs { 
            gameTiles  = newTiles, 
            gameUnits = newUnits }
        newTiles = Map.insert newLoc newToTile $ Map.insert oldLoc newFromTile oldTiles 
        newUnits = Map.insert uid newUnitState oldUnits
        unitState = (gameUnits gs) Map.! uid
        oldLoc = pos unitState
        newUnitState = unitState {pos = newLoc}
        oldTiles = gameTiles  gs
        oldUnits = gameUnits  gs
        newToTile = (oldTiles Map.! newLoc) {occupant = TileUnit uid}
        newFromTile = (oldTiles Map.! oldLoc) {occupant = Empty}
        
attackUnit :: UnitID -> UnitID -> GameState -> GameState
attackUnit uid targetID gs = 
    --Precond
    --TODO check in range
    --Postcond
    assert (getUnitHealth gs targetID > getUnitHealth ret targetID) $ 
    assert (getUnitPos gs uid == getUnitPos ret uid) $
    assert (getUnitPos gs targetID == getUnitPos ret targetID) $
    assert (validGameUnits ret) $
    assert (validGameTiles ret) $
    --TODO assert in range
    ret
    where 
        ret = gs { 
            gameUnits = newUnits }
        targetState = (gameUnits gs) Map.! targetID
        targetHealth = hp targetState
        attackerUnit = unitByID uid gs
        attackerType = unitType $ attackerUnit
        newAttacker = attackerUnit {lastFireTurn = turn gs} --Mark us as having fired this time
        newHealth = targetHealth - baseAttack attackerType
        newState = targetState {hp = newHealth, isAlive = (newHealth > 0)}
        oldTiles = gameTiles  gs
        oldUnits = gameUnits  gs
        newUnits = Map.insert uid newAttacker $ Map.insert targetID newState oldUnits

--TODO bad to pass Unit and Tile ID?
removeUnit :: UnitID -> TileID -> GameState -> GameState
removeUnit uid target gs = gs { 
    gameTiles  = newTiles, 
    gameUnits = newUnits }
    where 
        oldTiles = gameTiles  gs
        oldUnits = gameUnits  gs
        emptyTile = (oldTiles Map.! target) {occupant = Empty}   
        newTiles = Map.insert target emptyTile oldTiles 
        newUnits = Map.delete uid oldUnits


moveUnitAt :: TileID -> TileID -> GameState -> GameState
moveUnitAt from to gs = assert ((occupantAt to gs) == Empty) $
    gs {gameTiles  = newTiles}
    where 
        oldTiles = gameTiles  gs
        oldFromTile = oldTiles Map.! from
        oldToTile = oldTiles Map.! to
        unitToMove = occupantAt from gs
        newFromTile = oldFromTile {occupant = unitToMove}
        newToTile = oldToTile {occupant = Empty}
        intermTiles = Map.insert to newFromTile oldTiles
        newTiles = Map.insert to newToTile intermTiles        
        
        
fillUnitQueue :: GameState -> GameState
fillUnitQueue gs = gs {unitQueue = newQ}
    where
            gunits = gameUnits gs
            allUnits = Map.keys gunits
            cUnits = filter (\u -> unitTeam (gunits ! u) == TeamC) allUnits
            fUnits = filter (\u -> unitTeam (gunits ! u) == TeamF) allUnits
            (zl, cl, fl) = zipWithLeftover cUnits fUnits
            zlt = [((TeamC, x), (TeamF, y)) | (x,y) <- zl]
            clt = [(TeamC, x) | x <- cl]
            flt = [(TeamF, x) | x <- fl]
            newQ  = (concatPairs zlt) ++ clt ++ flt
            
setUnitQueue q gs = gs {unitQueue = q}

incrTurn gs = gs {turn = t + 1}
    where t = turn gs
    
teamOf :: GameState -> UnitID -> Team
teamOf gs uid = unitTeam (gameUnits gs ! uid)
