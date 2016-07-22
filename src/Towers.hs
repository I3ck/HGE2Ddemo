module Towers where

import Datas
import Guns
import Radars
import RangeFilters

import HGE2D.Datas

--------------------------------------------------------------------------------

towerLaser :: Tower
towerLaser = Tower
    { towerPos = TilePosition 0 0
    , towerHighlighted = False
    , base = Base1
    , center = Center1
    , gun = gunLaser
    , radar = radarLaser
    , creepSelector = SelectLowestHp
    , aimController = AimAtIntersection
    , rangeFilter = rangeFilter1
    }

--------------------------------------------------------------------------------

towerCannon :: Tower
towerCannon = Tower
    { towerPos = TilePosition 0 0
    , towerHighlighted = False
    , base = Base1
    , center = Center1
    , gun = gunCannon
    , radar = radarLaser
    , creepSelector = SelectLowestHp
    , aimController = AimAtIntersection
    , rangeFilter = rangeFilter1
    }

--------------------------------------------------------------------------------

towerSlow :: Tower
towerSlow = Tower
    { towerPos = TilePosition 0 0
    , towerHighlighted = False
    , base = Base1
    , center = Center1
    , gun = gunSlow
    , radar = radarLaser
    , creepSelector = SelectLeastSlowed
    , aimController = AimAtIntersection
    , rangeFilter = rangeFilter1
    }

--------------------------------------------------------------------------------
