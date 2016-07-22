module GameStates where

import Datas
import Settings
import Towers
import HigherOrder

import HGE2D.Datas

gameStateInitial :: GameState
gameStateInitial = GameState
    { gsSize      = (windowWidth, windowHeight)
    , time        = 0
    , level       = 1
    , lifes       = 50
    , gold        = 250
    , world       = World
                  { spawnPoint = TilePosition 0 8
                  , wayPoints  = []
                  , tileData   = []
                  }
    , buildPos    = Nothing
    , hoveredTile = Nothing
    , towers      = []
    , creeps      = []
    , shots       = []
    , idGuiTow    = 0
    , guiTowers   = positionGuiTowers $
        [ towerLaser
        , towerCannon
        , towerSlow
        ]
    }
