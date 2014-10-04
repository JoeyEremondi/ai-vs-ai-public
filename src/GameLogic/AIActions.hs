module GameLogic.AIActions where

import Prelude
import AiVsAi.GameData
import GameLogic.GameState
import GameLogic.GameStateMutators
import AiVsAi.Util
import AiVsAi.UnitProperties
import Data.Map ((!), keys, elems, member)
import qualified Data.Map as Map
--import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State

import GameLogic.Deltas

import AiVsAi.AIUtil

import Control.Exception (assert)

import Util.Debug (trace)

--import Data.Typeable.Internal (Typeable)
--import Data.Data (Data)

--import AIUtil


--TODO remove



--type StateMove = (Either Action Result, GameState)



{-
takeAction :: UnitID -> Action -> GameState -> GameState
takeAction uid (Move to) gs = moveUnitAt from to gs
    where
        units = gameUnits gs
        unit = units ! uid
        from = pos unit
takeAction uid (Fire targetTile) gs = error "TODO implement"
    where targetUnit = occupantAt targetTile gs
takeAction _uid Pass gs = gs
    -}
        
 
validActions :: UnitState -> GameState -> [Action]
--TODO firing
validActions unitState gs = validFires unitState gs ++ validMoves unitState gs ++ [Pass]
        
        
validMoves :: UnitState -> GameState -> [Action]      
validMoves unitState gs = 
    --Precond
    --Postcond
    assert (and $ map (actionIsValid uid gs) ret) $
    ret
    where
        ret = (map Move . (filter validMove)) allMoves
        uid = idNum unitState
        unitPos = pos unitState
        utype = unitType unitState
        allMoves = keys $ gameTiles gs
        validMove tileID = tileInRange tileID && occupantAt tileID gs == Empty
        --tileInRange tileID =  --(euclidDist tileID unitPos) <= movementRange utype
        tileInRange tileID = case (graphDistanceTo gs tileID unitPos) of
            Just d -> fromIntegral d <= movementRange utype
            Nothing -> False 
        
validFires :: UnitState -> GameState -> [Action]
validFires unitState gs = ((map $ Fire) . (filter validFire)) allFires
    where
        shootingUid = idNum unitState
        unitPos = pos unitState
        utype = unitType unitState
        allFires =  [uid | uid <- keys $  gameUnits gs]
        targetPos uid = pos $  (gameUnits gs) ! uid
        validFire target = unitInFiringRange gs (idNum unitState) target
            && (getUnitTeam gs target) /= unitTeam unitState
            
            
defaultAction :: ActionChoice
defaultAction _ actionList _ = getDefault $  filter notPass actionList
    where 
        getDefault [] = Pass
        getDefault (action:_) = action
        notPass x = x /= Pass




advanceState :: DefAction ->  State GameState ()
advanceState actionDone = do
    initState <- get
    let gunits = gameUnits initState
    applyAction actionDone
    intermState <- get --clean dead units
    forM (elems $  gameUnits intermState ) checkAlive
    modify $ incrTurn
    return ()


{-
nextState :: (Strategy, Strategy )-> State GameState (UnitState, Action)
--If we don't have a list of units to move, generate one
nextState  (cStrategy, fStrategy) = do
    initState <- get
    let gunits = gameUnits initState
    let q = unitQueue initState
    case q of
        [] -> do
            modify fillUnitQueue
            nextState (cStrategy, fStrategy)            
        ((TeamC, u):us) -> do
            let ustate = gunits ! u
            actionDone <- doAction cStrategy ustate
            intermState <- get --clean dead units
            forM (elems $  gameUnits intermState ) checkAlive
            modify $ incrTurn
            modify $ setUnitQueue us
            return (ustate, actionDone)
        ((TeamF, u):us) -> do
            let ustate = gunits ! u
            actionDone <- doAction fStrategy ustate
            intermState <- get --clean dead units
            forM (elems $ gameUnits intermState ) checkAlive
            modify $ incrTurn
            modify $ setUnitQueue us
            return (ustate, actionDone)
                
                {-l1 <- forM zl (alternateMoves (cStrategy, fStrategy))
                l2 <- forM cExtra (doAction cStrategy)
                l3 <- forM fExtra (doAction fStrategy)
                intermState <- get
                forM (elems $ gameUnits intermState ) checkAlive
                let ret = (concatPairs l1) ++ l2 ++ l3
                finalState <- get
                traceShow (map fst ret) 
                    $  return (Left Pass, finalState) -}

-}
    
checkAlive :: UnitState -> State GameState ()
checkAlive u 
    | isAlive u = return ()
    | otherwise = do 
        modify $  removeUnit (idNum u) (pos u)
        return ()

{-
alternateMoves ::  (Strategy, Strategy ) -> (UnitState, UnitState) -> State GameState (StateMove, StateMove)
alternateMoves  (sc, sf) (uc, uf) = do
    doAction sc uc
    s1 <- get
    doAction sf uf 
    s2 <- get
    return ((Left Pass, s1), (Left Pass, s2))
    -}
    
doAction :: Strategy -> UnitState ->  State GameState DefAction
doAction s u = do
        let utype = unitType u
        let actionChoice = s utype
        gs <- get
        let actionList =  validActions u gs
        let moveToDo = toDefiniteAction gs (idNum u) $ actionChoice u actionList gs
        applyAction moveToDo
        return moveToDo

--TODO fix redundant
applyAction ::  DefAction -> State GameState ()
applyAction  (DefMove uid loc) = do
    gs <- get
    let ustate = unitByID uid gs
    modify $  moveUnit (idNum ustate) loc
applyAction (DefFire shooter target ) = do
    gs <- get
    let ustate = unitByID shooter gs
    modify $  attackUnit (idNum ustate) target
applyAction (DefPass) = return ()

--TODO redundant
--TODO add deltas
gameAfterAction :: GameState -> DefAction ->  GameUpdate
gameAfterAction gs  action = 
    let 
      newgs =  execState (advanceState action) gs 
      deltas = deltasFromAction gs action
    in
      case (result newgs) of
          Nothing -> assert (validGameUnits newgs) $
              assert (validGameTiles newgs) $
              InProgress newgs deltas
          (Just result) -> Finished newgs result
          
          --TODO change result in elm

--Return an infinite list of the GameUpdates happening after a given state

{-
infStateList :: GameState -> (Strategy, Strategy ) -> [Deltas.GameUpdate]
infStateList gs strats = helper gs strats (result gs)
    where
        helper gs _ (Just result) = [Deltas.Finished result]
        helper gs strats Nothing = (Deltas.InProgress gs []) : statesAfter
        (_actionDone, theNextState) = runState (nextState strats) gs
        statesAfter = infStateList theNextState strats
-}

toDefiniteAction :: GameState -> UnitID -> Action -> DefAction
toDefiniteAction _ uid (Move a) = DefMove uid a
toDefiniteAction _ uid (Fire a) = DefFire uid a
toDefiniteAction _ _ Pass = trace "Converting pass" $ DefPass  
toDefiniteAction gs uid (ChaseOrFireAt target) =
    --Precond
    assert (member target (gameUnits gs))
    assert (member uid (gameUnits gs))
    --Postcond
    --assert (actionIsValid uid gs ret) 
    ret
    where
        ret | weaponIsCharged gs uid && unitInFiringRange gs uid target = 
                assert (actionIsValid uid  gs (Fire target))$
                DefFire uid target
            | otherwise = 
                toDefiniteAction gs uid $ MoveTowards  tpos
        tstate = (gameUnits gs) ! target
        tpos = pos tstate
--TODO make sure empty
toDefiniteAction gs uid (MoveTowards target) = theMove --TODO take range into account
    where
        upos =  (getUnitPos gs uid) --TODO look at range
        theMove = case (shortestPath gs upos target) of
            Just (otherUPos : h:  _) ->
                --trace ("Game State " ++ (show gs)) $
                --trace ("trying to move unit " ++ (show $ (gameUnits gs) ! uid)) $
                --trace ("trying to move from " ++ (show upos) ++ " to " ++ (show h) ) $
                --trace ("tile is empty? " ++ (show $ tileEmpty h gs) ++ "\n") $
                --trace ("tile contents " ++ (show $ occupantAt h gs) ++ "\n") $
                --trace ("list head " ++ show otherUPos) $
                --trace ("upos" ++ (show upos) ++ " move to " ++ (show h)) $
                --trace ("In range " ++ (show $ tileInRange gs uid h))$
                assert (actionIsValid uid  gs (Move h)) $
                DefMove uid h
            _ -> DefPass
toDefiniteAction gs u (Retreat) = DefPass --TODO fix


deltasFromAction :: GameState -> DefAction -> [Delta]
deltasFromAction _gs (DefMove uid tid) = [DMove uid tid] --TODO implement
deltasFromAction gs (DefFire uid target) = [DFire uid (getUnitPos gs uid) targetPos, DHit target 1 ] ++ maybeDead
    where
        targetPos = getUnitPos gs target
        --TODO more than base?
        maybeDead = if (getUnitHealth gs target) <= (baseAttack $ getUnitType gs uid) then
            [DDead target (getUnitPos gs target)]
            else []
deltasFromAction _ (DefPass) = []        
        
actionIsValid :: UnitID -> GameState -> Action -> Bool
actionIsValid uid gs (Move tid) = tileEmpty tid gs && tileInRange gs uid tid
actionIsValid uid gs (Fire target) = weaponIsCharged gs uid && (member target (gameUnits gs) ) && unitInFiringRange gs uid target
actionIsValid _ _ (Pass) = True
--TODO check for path
actionIsValid uid gs (ChaseOrFireAt target) = member target (gameUnits gs)
actionIsValid _ _ _ = True

--TODO merge with enemyInRange

tileInRange gs uid tid = inRange
    where
        unitState = (gameUnits gs) ! uid
        unitPos = pos unitState
        utype = unitType unitState
        inRange  = case (distanceToTile gs uid tid) of
            Just d -> d <= movementRange utype
            _ -> False

unitInFiringRange gs uid target = trace ("Unit in range? " ++ (show inRange) ++ " pos" ++ (show unitPos) ++ "target " ++ (show targetLoc) ++ "dist " ++ (show maybeDist)) inRange
    where
        targetLoc = pos $ (gameUnits gs) ! target
        unitState = (gameUnits gs) ! uid
        unitPos = pos unitState
        utype = unitType unitState
        maybeDist = (euclidDistanceToUnit gs uid target)
        inRange  = case maybeDist of
            Just d -> d <= attackRange utype
            _ -> False

--Make sure enough time has passed since the last time we discharged our weapon    
weaponIsCharged gs uid = trace ("Weapon charged? " ++ show ret) ret
  where
    ret = currentTurn - lastFire >= chargeTime
    ustate = (gameUnits gs) ! uid
    chargeTime = fireRechargeTime $ unitType ustate
    currentTurn = turn gs
    lastFire = lastFireTurn ustate
    
--TODO expand
commandIsDone gs uid (Just (MoveTowards pos)) = case (distanceToTile gs uid pos) of
  Nothing -> True --If can't get to dest, give up
  Just _ -> (pos == getUnitPos gs uid) --If at dest, then stop
  
commandIsDone gs uid (Just (ChaseOrFireAt target)) = case (Map.lookup target $ gameUnits gs) of
  Nothing -> True --Quit when target is dead
  _ -> False

commandIsDone gs uid action = False
