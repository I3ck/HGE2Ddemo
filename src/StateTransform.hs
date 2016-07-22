module StateTransform where

import Settings
import Datas
import Valuable
import Geometry
import HigherOrder
import HGE2DInstances ()

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Geometry
import HGE2D.Collision

import Data.List

myClick x y gs
    | clickedMap && enoughGold && canBePlaced = gs { towers = newTower : (towers gs), gold = newGold, buildPos = newBuildPos }
    | clickedSidebar = gs { idGuiTow = newSelectedId, buildPos = newBuildPos }
    | otherwise = gs { buildPos = newBuildPos }
    where
      newTower    = (guiTowers gs !! idGuiTow gs ) { towerPos = newTilePos}
      newTilePos  = toTilePos $ (x, y)

      newBuildPos | clickedMap && not clickedTower = Just newTilePos
                  | otherwise = Nothing

      newGold     = (gold gs) - (valueBuy newTower)
      enoughGold  = (gold gs) >= valueBuy newTower

      clickedTower    = any onTilePos (towers gs) ---TODO defined several times
      clickedMap      = x > 0 && x < mapSizeX && y > 0 && y < mapSizeY
      canBePlaced     = canBeBuiltOn ( getTile newTilePos (world gs) ) && ( not clickedTower )

      clickedSidebar  = x > mapSizeX && x < totalSizeX

      newSelectedId   = getGuiTowerId x y gs

      ---TODO used in hover and clicked, define somewhere more general
      onTilePos :: Tower -> Bool
      onTilePos t = newTilePos == (towerPos t)


myHover x y gs = gs { towers = towersRetoggled, hoveredTile = hovered }
    where
      towersRetoggled = (map enableIfHover . map disable) (towers gs)
      clickedMap      = x > 0 && x < mapSizeX && y > 0 && y < mapSizeY

      newTilePos = toTilePos $ (x, y) ---TODO make maybe

      hovered | clickedMap = Just newTilePos
              | otherwise = Nothing

      disable :: Tower -> Tower
      disable t = t { towerHighlighted = False }

      enableIfHover :: Tower -> Tower
      enableIfHover t | onTilePos t = t { towerHighlighted = True }
                      | otherwise = t

      ---TODO used in hover and clicked, define somewhere more general
      onTilePos :: Tower -> Bool
      onTilePos t = newTilePos == (towerPos t)

myMUp _ _ = id
myDrag _ _ = id
myKeyUp _ _ _ = id
myKeyDown _ _ _ = id

--------------------------------------------------------------------------------

---TODO make sure that the expensive operations are done first
transformationState :: Millisecond -> GameState -> GameState
transformationState ms gs = fireShots
                          $ applyAi
                          $ updateCreepDirections
                          $ updateWaypointsOfCreeps
                          $ removeDeadCreeps
                          $ removeCollidingShots
                          $ addGoldOfKilledCreeps
                          $ addBurnToCreepsHitByBurning
                          $ addSlowToCreepsHitBySlowing
                          $ removeLifeOfCreepsHit
                          $ removeShotsOutOfRange
                          $ removeShotsOutside
                          $ removeCreepsOutside
                          $ gs { towers = map (moveInTime ms) (towers gs), creeps = map (moveInTime ms) (creeps gs), shots = map (moveInTime ms) (shots gs) } ---TODO define as function


--------------------------------------------------------------------------------

---TODO move somewhere else
isAlive :: Creep -> Bool
isAlive c = (hp c) > 0

updateWaypointsOfCreeps :: GameState -> GameState
updateWaypointsOfCreeps gs = gs { creeps = map updateWayPoints (creeps gs) }
  where
    updateWayPoints :: Creep -> Creep
    updateWayPoints c = c { targetWayPoint = newWaypoint }
      where
        newWaypoint | (targetWayPoint c) >= (length $ wayPoints $ world gs) - 1 = (targetWayPoint c)
                    | hittingNextWayPoint = (targetWayPoint c) + 1
                    | otherwise = targetWayPoint c
        hittingNextWayPoint = isInside c ((wayPoints $ world gs) !! (targetWayPoint c))


updateCreepDirections :: GameState -> GameState
updateCreepDirections gs = gs { creeps = map (setCreepDirection (wayPoints $ world gs)) (creeps gs)}

removeShotsOutOfRange :: GameState -> GameState
removeShotsOutOfRange gs = gs { shots = newShots }
  where
    newShots = filter inRange (shots gs)

    inRange :: Shot -> Bool
    inRange s = (shotRange s) >= distance (shotStart s) (getPos s)


removeDeadCreeps :: GameState -> GameState
removeDeadCreeps gs = gs { creeps = newCreeps }
  where
    newCreeps = filter isAlive (creeps gs)


addGoldOfKilledCreeps :: GameState -> GameState
addGoldOfKilledCreeps gs = gs { gold = newGold }
  where
    newGold = (gold gs) + addedGold
    addedGold = sum killedValues
    killedValues = map creepValue killedCreeps
    killedCreeps = filter (not . isAlive) (creeps gs)

addBurnToCreepsHitByBurning :: GameState -> GameState
addBurnToCreepsHitByBurning gs = gs { creeps = newCreeps}
  where
    newCreeps = map burnIfHitByBurning (creeps gs)

    burnIfHitByBurning :: Creep -> Creep
    burnIfHitByBurning c = newCreep
      where
        newCreep = applyAllPossibleBurns (shotsHitting c (shots gs)) c

    applyAllPossibleBurns :: [Shot] -> Creep -> Creep
    applyAllPossibleBurns shots c = foldr foldBurn c shots

    foldBurn :: Shot -> Creep -> Creep
    foldBurn s c | dpsDur s == 0 = c
                 | dps s == 0 = c
                 | otherwise = burnCreep (dps s) (dpsDur s) c

addSlowToCreepsHitBySlowing :: GameState -> GameState
addSlowToCreepsHitBySlowing gs = gs { creeps = newCreeps }
  where
    newCreeps = map slowIfHitBySlowing (creeps gs)

    slowIfHitBySlowing :: Creep -> Creep
    slowIfHitBySlowing c = newCreep
      where
        newCreep = applyAllPossibleSlows (shotsHitting c (shots gs)) c

    applyAllPossibleSlows :: [Shot] -> Creep -> Creep
    applyAllPossibleSlows shots c = foldr foldSlow c shots

    foldSlow :: Shot -> Creep -> Creep
    foldSlow s c | slowDur s == 0 = c
                 | slow s == 1 = c
                 | otherwise = slowCreep (slow s) (slowDur s) c

removeLifeOfCreepsHit :: GameState -> GameState
removeLifeOfCreepsHit gs = gs { creeps = newCreeps }
  where
    newCreeps = map removeLifeIfHit (creeps gs)

    removeLifeIfHit :: Creep -> Creep
    removeLifeIfHit c = c { hp = newHp }
      where
        newHp = (hp c) - totalDamage
        damagesOfHitting = map shotDamage (shotsHitting c (shots gs))
        totalDamage = sum damagesOfHitting

fireShots :: GameState -> GameState
fireShots gs = gs { shots = allShots, towers = newTowers }
  where
    allShots = newShots ++ (shots gs)
    newShots = map fireShot $ filter canShoot (towers gs)
    newTowers = map reloadIfCanShoot (towers gs)

    canShoot :: Tower -> Bool
    canShoot t = (gunEnabled $ gun t) && (reloadTime $ gun t) == 0

    reloadIfCanShoot :: Tower -> Tower
    reloadIfCanShoot t
        | not $ canShoot t = t
        | otherwise = t { gun = reloadedGun $ gun t}
          where
            reloadedGun :: Gun -> Gun
            reloadedGun g = g { reloadTime = round $ 1.0 / fireRate g}

--------------------------------------------------------------------------------

removeShotsOutside :: GameState -> GameState
removeShotsOutside gs = gs { shots = newShots }
  where
    newShots = filter isInside (shots gs)

    isInside :: Shot -> Bool
    isInside shot = inX && inY
     where
      inX = sX >= 0 && sX <= mapSizeX
      inY = sY >= 0 && sY <= mapSizeY
      sX = getX shot
      sY = getY shot

--------------------------------------------------------------------------------

removeCollidingShots :: GameState -> GameState
removeCollidingShots gs = gs { shots = newShots }
  where
    newShots    = filter collidseWithNone (shots gs)
    rigidCreeps = map creepRB (creeps gs)

    collidseWithNone :: Shot -> Bool
    collidseWithNone s = not $ any (doCollide s) rigidCreeps

--------------------------------------------------------------------------------

removeCreepsOutside :: GameState -> GameState
removeCreepsOutside gs = gs { creeps = newCreeps, lifes = newLifes }
  where
    newLifes        = (lifes gs) - creepsOutSide
    creepsOutSide   = length $ filter creepOutSide (creeps gs)
    newCreeps       = map placeLeftIfOutside (creeps gs) -- filter (not . creepOutSide) (creeps gs)

    creepOutSide :: Creep -> Bool
    creepOutSide creep =    (getX creep > mapSizeX)
                         || (getX creep < 0)
                         || (getY creep > mapSizeY)
                         || (getY creep < 0)

    placeLeftIfOutside :: Creep -> Creep
    placeLeftIfOutside creep = newCreep
      where
        newCreep | getX creep <= mapSizeX = creep
                 | otherwise = moveTo (0, (getY creep) ) creep { targetWayPoint = 1}

--------------------------------------------------------------------------------

applyAi :: GameState -> GameState ---TODO rename
applyAi gs = gs { towers = map (towerAi $ creeps gs) (towers gs) }
  where
    towerAi :: [Creep] -> Tower -> Tower
    towerAi creeps t = aimAt aim t { gun = (gun t) { gunEnabled = fire } }
      where
        aim = decideAim (aimController t)
            $ selectCreep (creepSelector t)
            $ filterByRange (rangeFilter t)
            $ filterByRadar (radar t) creeps

        fire = decideFire
            $ selectCreep (creepSelector t)
            $ filterByRange (rangeFilter t)
            $ filterByRadar (radar t) creeps

        filterByRadar :: Radar -> [Creep] -> [Creep]
        filterByRadar r creeps = filter inRange creeps ---TODO apply filtering logic later
          where
            inRange :: Creep -> Bool
            inRange c = distance (getPos t) (getPos c) <= scanRange r

        filterByRange :: RangeFilter -> [Creep] -> [Creep]
        filterByRange _ creeps = creeps --- TODO must be defined in tower space to know the distance


        selectCreep :: CreepSelector -> [Creep] -> Maybe Creep
        selectCreep _ [] = Nothing
        selectCreep selector creeps =
            case selector of
                SelectFirst            -> Just $ head creeps
                SelectClosest          -> Just nearest
                SelectLowestHp         -> Just lowestHp
                SelectHighestHp        -> Just highestHp
                SelectFastest          -> Just fastest
                SelectSlowest          -> Just slowest
                SelectMostSlowed       -> Just mostSlowed
                SelectLeastSlowed      -> Just leastSlowed
                SelectMostBurning      -> Just mostBurning
                SelectLeastBurning     -> Just leastBurning
          where
            nearest         = minimumBy sortDist     creeps
            lowestHp        = minimumBy sortHp       creeps
            highestHp       = maximumBy sortHp       creeps
            slowest         = minimumBy sortSpeed    creeps
            fastest         = maximumBy sortSpeed    creeps
            leastSlowed     = minimumBy sortSlow     creeps
            mostSlowed      = maximumBy sortSlow     creeps
            leastBurning    = minimumBy sortBurn     creeps
            mostBurning     = maximumBy sortBurn     creeps

            sortDist :: Creep -> Creep -> Ordering
            sortDist c1 c2 = compare dist1 dist2
              where
                dist1 = (distance (getPos t) (getPos c1)) ---TODO can use sqrdist here
                dist2 = (distance (getPos t) (getPos c2)) ---TODO can use sqrdist here

            sortHp :: Creep -> Creep -> Ordering
            sortHp c1 c2 = compare (hp c1) (hp c2)

            sortSpeed :: Creep -> Creep -> Ordering
            sortSpeed c1 c2 = compare (speed c1) (speed c2)

            sortSlow :: Creep -> Creep -> Ordering
            sortSlow c1 c2 = compare (slowDuration c1) (slowDuration c2)

            sortBurn :: Creep -> Creep -> Ordering
            sortBurn c1 c2 = compare (burnDuration c1) (burnDuration c2)

        decideAim :: AimController -> Maybe Creep -> RealPosition
        decideAim _ Nothing = aimDown t
        decideAim ac (Just creep) =
            case ac of
                AimAt00                -> (0, 0)
                AimDirectly            -> getPos creep
                AimAtIntersection      -> interceptionPos
                                              (toRealPos (towerPos t),      shotSpeed $ shot $ gun t ) ---TODO aims incorrectly since using tower upper left?
                                              (getPos creep,  (slowedVelX, slowedVelY))
                  where
                    slowedVelX = 1.0 / (slowFactor creep) * (fst $ rigidVel $ creepRB $ creep)
                    slowedVelY = 1.0 / (slowFactor creep) * (snd $ rigidVel $ creepRB $ creep)

        decideFire :: Maybe Creep -> Bool
        decideFire Nothing       = False
        decideFire (Just _ )     = True

--------------------------------------------------------------------------------


---TODO define somewhere in helpers or higher order
hittingCreep :: Creep -> Shot -> Bool --- TODO already reused, define further out and add Creep as parameter then
hittingCreep c s = doCollide s c

shotsHitting :: Creep -> [Shot] -> [Shot]
shotsHitting c shots = filter (hittingCreep c) shots
