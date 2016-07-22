module HGE2DInstances where

import Datas
import Geometry

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Geometry
import HGE2D.Classes
import HGE2D.Instances ()

--------------------------------------------------------------------------------

instance Dynamic Tower where
    moveInTime time t = t { gun = moveInTime time (gun t), radar = moveInTime time (radar t) }

--------------------------------------------------------------------------------

instance Dynamic Gun where
    moveInTime time g = g { reloadTime = newReloadTime (reloadTime g) time}
        where
            newReloadTime reloadTime time  | reloadTime - time > 0 = reloadTime - time
                                           | otherwise             = 0

--------------------------------------------------------------------------------

instance Dynamic Radar where
    moveInTime _ r = r --- TODO save last scan time and decide when it can scan again etc.

--------------------------------------------------------------------------------

instance Dynamic Creep where ---TODO warning, currently rounding
    moveInTime time c = c
                       { creepRB = moveInTime slowedTime (creepRB c)
                       , hp = newHp
                       , burnDuration = newBurnDuration
                       , slowDuration = newSlowDuration
                       }
      where
        newBurnDuration = max 0 (burnDuration c - maxBurnTime)
        newHp           = (hp c) -  ( (fromIntegral maxBurnTime) * burnAmount c) / 1000
        maxBurnTime     = min time (burnDuration c)

        newSlowDuration = max 0 (slowDuration c - maxSlowTime)
        maxSlowTime     = min time (slowDuration c)
        slowedTime      | newSlowDuration > 0 = round $ (fromIntegral time) / (slowFactor c)
                        | otherwise = time

--------------------------------------------------------------------------------

instance Dynamic Shot where
    moveInTime time s = s { shotRB = moveInTime time (shotRB s) }

--------------------------------------------------------------------------------

instance HasBoundingBox Shot where
    getBB s = getBB $ shotRB s


instance HasBoundingBox Creep where
    getBB c = getBB $ creepRB c


--------------------------------------------------------------------------------

instance Positioned TilePosition where ---TODO define in engine?
    getPos tp = getPos $ toRealPosCentered tp
    getX tp = getX $ toRealPosCentered tp
    getY tp = getY $ toRealPosCentered tp


instance Positioned Creep where
    getPos c = getPos $ creepRB c
    getX c = getX $ creepRB c
    getY c = getY $ creepRB c

instance Positioned Shot where
    getPos s = getPos $ shotRB s
    getX s = getX $ shotRB s
    getY s = getY $ shotRB s

instance Positioned Tower where
    getPos t = getPos $ towerPos t
    getX t = getX $ towerPos t
    getY t = getY $ towerPos t

--------------------------------------------------------------------------------

instance Moveable Creep where
    moveBy by c = c { creepRB = moveBy by (creepRB c) }
    moveTo to c = c { creepRB = moveTo to (creepRB c) }


instance Moveable Shot where
    moveBy by s = s { shotRB = moveBy by (shotRB s) }
    moveTo to s = s { shotRB = moveTo to (shotRB s) }

--------------------------------------------------------------------------------
