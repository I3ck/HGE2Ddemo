module Shots where

import Datas

import HGE2D.Datas
import HGE2D.Geometry

shotCannon :: Shot
shotCannon = Shot
    { shotSkin = ShotSkinCannon
    , shotStart = (0, 0) --- TODO make parameter
    , shotRB = makeRB (0, 0) (10.0, 5.0) 2 2
    , shotDamage = 500
    , shotRange = 200
    , shotSpeed = 0.05
    , splash = 0
    , dps = 0
    , dpsDur = 0
    , slow = 0
    , slowDur = 0
    , hitsAir = False
    , hitsMany = False
    }

--------------------------------------------------------------------------------

shotLaser :: Shot
shotLaser = Shot
    { shotSkin = ShotSkinLaser
    , shotStart = (0, 0) --- TODO make parameter
    , shotRB = makeRB (0, 0) (0.0, 0.0) 2 2
    , shotDamage = 100
    , shotRange = 300
    , shotSpeed = 0.25
    , splash = 0
    , dps = 0
    , dpsDur = 0
    , slow = 0
    , slowDur = 0
    , hitsAir = False
    , hitsMany = False
    }

--------------------------------------------------------------------------------

shotSlow :: Shot
shotSlow = Shot
    { shotSkin = ShotSkinSlow
    , shotStart = (0, 0) --- TODO make parameter
    , shotRB = makeRB (0, 0) (0.0, 0.0) 2 2
    , shotDamage = 0
    , shotRange = 300
    , shotSpeed = 0.15
    , splash = 0
    , dps = 0
    , dpsDur = 0
    , slow = 1.5
    , slowDur = 2000
    , hitsAir = False
    , hitsMany = False
    }

--------------------------------------------------------------------------------
