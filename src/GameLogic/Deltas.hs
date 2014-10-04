module GameLogic.Deltas where

import Prelude
import AiVsAi.GameData
import Control.Concurrent.STM


--Definite actions for unit movements
--For example, the AI might specify "attack closest enemy"
--Which is then translated into a definite attack at a specifc spot
--The calculation and execution of a DefAction are a single atomic operation in STM
--Ensuring that simultaneous changes don't caues invalid actions
data DefAction = 
    DefMove UnitID TileID
    |DefFire UnitID UnitID
    |DefPass 
    deriving (Eq, Show, Read)



--A Delta is a game state transformation, which can be sent to the Frontend
--It should be possible to compute the current gamestate entirely from the original game state and the deltas
--TODO what if they get sent out of order?
data Delta = DMove UnitID TileID
    | DFire UnitID TileID TileID
    | DHit UnitID Int
    | DDead UnitID TileID
    -- | GameOver Result
    deriving (Show)
    
--An update of the current game's Meta state (i.e. started, in progress, over, etc.)
--Can be sent to the Frontend
data GameUpdate = NotStarted | InProgress GameState [Delta] | GameError String | Finished GameState Result
  deriving (Show)

--Simple helper to test if a game is done
isGameDone :: GameUpdate -> Bool
isGameDone (Finished _ _) = True
isGameDone _ = False