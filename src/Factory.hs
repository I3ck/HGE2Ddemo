module Factory where

import Types
import Datas
import Settings
import HGE2DInstances ()

import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Instances ()
import HGE2D.Geometry

import System.Random

--------------------------------------------------------------------------------

--- TODO also alternate speeds
---TODO seems like it was able spawn creeps outside of map, make sure this doesnt happen
makeWave :: BoundingBox -> Int -> Creep -> [Creep]
makeWave bb nCreeps creep = map makeCreep [0..nCreeps-1]
  where
      makeCreep :: Int -> Creep
      makeCreep i = moveTo (newX, newY)  (creep { creepRB = (creepRB creep) { rigidVel = newVel } })
        where
          newX      = (fst $ bbMin bb) + (rndNumsW !! i)
          newY      = (snd $ bbMin bb) + (rndNumsH !! i)
          newVel    = ((speedF * fst oldVel), (speedF * snd oldVel))
          speedF    = (rndSpeeds !! i) / 100
          oldVel    = rigidVel $ creepRB creep

      seedW = 1337
      seedH = 1338
      seedS = 1339

      rndNumsW  = randomRs (creepSpawnMargin,w) (mkStdGen seedW)
      rndNumsH  = randomRs (creepSpawnMargin,h) (mkStdGen seedH)
      rndSpeeds = randomRs (100, 125)           (mkStdGen seedS)

      w = wTile - (wCreep / 2) - (2 * creepSpawnMargin)
      h = hTile - (hCreep / 2) - (2 * creepSpawnMargin)

      wCreep = fst $ sizeBB $ rigidBB $ creepRB creep
      hCreep = snd $ sizeBB $ rigidBB $ creepRB creep

      wTile = fst $ sizeBB bb
      hTile = snd $ sizeBB bb

--------------------------------------------------------------------------------

---TODO use types for speed and value here once defined
makeCreep :: CreepSkin -> RigidBody -> Hp -> Double -> Double -> Bool -> Creep
makeCreep skin rb hp speed value canFly =
    Creep
    { creepSkin = skin
    , creepRB = rb
    , initHp = hp
    , hp = hp
    , speed = speed
    , creepValue = value
    , slowFactor = 1
    , slowDuration = 0
    , burnAmount = 0
    , burnDuration = 0
    , targetWayPoint = 1
    , canFly = canFly
    }

--------------------------------------------------------------------------------

makeGun :: GunSkin -> Double -> Shot -> Gun
makeGun skin fireRate shot =
    Gun
    { gunSkin = skin
    , reloadTime = 0
    , fireRate = fireRate
    , shot = shot
    , radGun = 0
    , gunEnabled = False
    }
