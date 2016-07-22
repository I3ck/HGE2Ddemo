module Main where

import Settings
import Datas
import HigherOrder
import Factory
import Creeps
import GameStates
import GlRender
import StateTransform

import HGE2D.Datas
import HGE2D.Engine

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do
    gs <- initGameState

    let
        myGetTitle _ = title
        myGetTime = time
        mySetTime ms gs = gs { time = ms }
        myMoveTime ms gs = transformationState ms gs
        myResize (w, h) gs = gs { gsSize = (realToFrac w, realToFrac h) }
        myGetSize = gsSize
        myToGlInstr gs = renderGs es gs

        es = EngineState
             { getTitle = myGetTitle
             , getTime = myGetTime
             , setTime = mySetTime
             , moveTime = myMoveTime
             , click = myClick
             , mUp = myMUp
             , hover = myHover
             , drag = myDrag
             , kDown = myKeyDown
             , kUp = myKeyUp
             , resize = myResize
             , getSize = myGetSize
             , toGlInstr = myToGlInstr
             } :: EngineState GameState

    runEngine es gs

--------------------------------------------------------------------------------

initGameState :: IO (GameState)
initGameState = do
    strW1 <- readFile "worlds/world1.world"

    let tiles1 = stringToMap $ strW1
        world1 = World
                 { spawnPoint = TilePosition 0 8
                 , wayPoints  = sortWaypoints $ generateWaypoints tiles1
                 , tileData   = tiles1
                 }
        wave1  = makeWave (generateSpawn tiles1 ) 10  creep1

    return (gameStateInitial {world = world1, creeps = wave1 })


---TODO move below ones to engine files
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
