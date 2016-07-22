module HigherOrder where

import Settings
import Types
import Datas
import Geometry
import HGE2DInstances ()

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Geometry

import Data.List

--------------------------------------------------------------------------------

---TODO either split into files or properly seperate here
---TODO rename to utils and maybe split into more files depending on which types they work on

---TODO maybe rename
aimDown :: Tower -> RealPosition
aimDown t = (newX, newY)
  where
    newX =       getX t
    newY = 0.1 + getY t

--------------------------------------------------------------------------------

burnCreep :: Dps -> Millisecond -> Creep -> Creep
burnCreep dps duration c = c { burnAmount = dps, burnDuration = newDuration }
  where
    newDuration = max duration (burnDuration c)

--------------------------------------------------------------------------------

slowCreep :: Slow -> Millisecond -> Creep -> Creep
slowCreep slow duration c = c { slowFactor = slow, slowDuration = newDuration }
  where
    newDuration = max duration (slowDuration c)

--------------------------------------------------------------------------------

aimAt :: RealPosition -> Tower -> Tower
aimAt pos t = t { gun = gunAim (gun t) }
  where
    gunAim :: Gun -> Gun
    gunAim g = g { radGun = radRealPos (toRealPosCentered (towerPos t)) pos}

rotateAim :: Radian -> Tower -> Tower
rotateAim phi t = t { gun = rot (gun t) }
  where
    rot :: Gun -> Gun
    rot g = g { radGun = (radGun g) + phi}

--------------------------------------------------------------------------------

fireShot :: Tower -> Shot
fireShot t = fire $ gun t
  where
    fire :: Gun -> Shot
    fire g = (shot g)
        { shotStart = (newX, newY)
        , shotRB = makeRB (newX, newY) (((*speed) $ cos $ radGun g), ((*speed) $ sin $ radGun g)) 2 2
        }
      where
        newX = (fst tileCorner) + (tileSize / 2)
        newY = (snd tileCorner) + (tileSize / 2)
        tileCorner = toRealPos (towerPos t)
        speed = shotSpeed $ shot $ gun t

--------------------------------------------------------------------------------

canBeBuiltOn :: Tile -> Bool
canBeBuiltOn Grass = True
canBeBuiltOn _ = False

--------------------------------------------------------------------------------

getTile :: TilePosition -> World -> Tile
getTile pos  w = (tileData w) !! (tileY pos) !! (tileX pos)

--------------------------------------------------------------------------------

mapToString :: [[Tile]] -> String
mapToString m = unlines $ map tilesToString m
  where
    tilesToString :: [Tile] -> String
    tilesToString tiles = map tileToString tiles

    tileToString :: Tile -> Char
    tileToString Water  = 'w'
    tileToString Grass  = 'g'
    tileToString Rock   = 'r'
    tileToString Forest = 'f'
    tileToString Path   = 'p'
    tileToString Spawn  = 's'

stringToMap :: String -> [[Tile]]
stringToMap s = map stringToTiles (lines s)
  where
    stringToTiles :: String -> [Tile]
    stringToTiles s = map charToTile s

    charToTile :: Char -> Tile
    charToTile 'w' = Water
    charToTile 'g' = Grass
    charToTile 'r' = Rock
    charToTile 'f' = Forest
    charToTile 'p' = Path
    charToTile 's' = Spawn
    charToTile _   = undefined ---TODO return Nothing here?

--------------------------------------------------------------------------------

positionGuiTowers :: [Tower] -> [Tower]
positionGuiTowers towers = map position (zip [0..] towers)
  where
    position :: (Int, Tower) -> Tower
    position (i, t) = t { towerPos = TilePosition mapTilesX i }

--------------------------------------------------------------------------------

getGuiTowerId :: Pixel -> Pixel -> GameState -> Int
getGuiTowerId x y gs | y > tileSize * (fromIntegral $ length $ guiTowers gs) = idGuiTow gs
                     | x > mapSizeX + tileSize                               = idGuiTow gs
                     | otherwise                                             = (ceiling $ y / tileSize) - 1

--------------------------------------------------------------------------------

rowSize :: [[Tile]] -> Int
rowSize tiles = length $ tiles !! 0

--------------------------------------------------------------------------------

indexTiles :: [[Tile]] -> [(Int, Tile)]
indexTiles tiles = zip [0..] $ concat $ tiles

--------------------------------------------------------------------------------

tilePos :: [[Tile]] -> Int -> TilePosition
tilePos tiles index = TilePosition (index `mod` rowSize tiles) (index `quot` rowSize tiles)

--------------------------------------------------------------------------------

---TODO below 6 unused
indexToTilePos :: (Int, Int) -> TilePosition
indexToTilePos (ix, iy) = TilePosition ix iy

indexRight :: (Int, Int) -> (Int, Int)
indexRight (ix, iy) = (ix+1, iy)

indexLeft :: (Int, Int) -> (Int, Int)
indexLeft (ix, iy) = (ix-1, iy)

indexAbove :: (Int, Int) -> (Int, Int)
indexAbove (ix, iy) = (ix, iy-1)

indexBelow :: (Int, Int) -> (Int, Int)
indexBelow (ix, iy) = (ix, iy+1)

indexValid :: [[Tile]] -> (Int, Int) -> Bool
indexValid tiles (ix, iy) =
    (length tiles) > 0 &&
    ix > 0 &&
    iy > 0 &&
    ix < (length tiles) &&
    iy < (length $ tiles !! 0)

--------------------------------------------------------------------------------

matchTiles :: Tile -> (Int, Tile) -> Bool
matchTiles comp (_, other) = comp == other

--------------------------------------------------------------------------------

generateWaypoints :: [[Tile]] -> [WayPoint]
generateWaypoints tiles = [spawnBox] ++ waypoints
  where
    spawnBox  = generateSpawn tiles
    waypoints = map pos2wp pathRealPositions

    pathRealPositions = map toRealPosCentered pathTilePositions
    pathTilePositions = map (tilePos tiles) pathIndexes
    pathIndexes       = map fst pathTiles
    pathTiles         = filter (matchTiles Path) (indexTiles tiles)

    pos2wp :: RealPosition -> WayPoint
    pos2wp pos = makeBB pos tileSize tileSize

sortWaypoints :: [WayPoint] -> [WayPoint]
sortWaypoints waypointsNSpawn = buildSorted [spawn] waypoints
  where
    spawn     = head waypointsNSpawn
    waypoints = tail waypointsNSpawn

    buildSorted :: [WayPoint] -> [WayPoint] -> [WayPoint]
    buildSorted sorted []     = sorted
    buildSorted sorted toSort = buildSorted newSorted newToSort
      where
        newSorted       = sorted ++ [head toSortByClosest]
        newToSort       = tail toSortByClosest
        toSortByClosest = sortBy compDistanceLastSorted toSort

        compDistanceLastSorted :: WayPoint -> WayPoint -> Ordering
        compDistanceLastSorted bb1 bb2 = compare (distance posSorted pos1) (distance posSorted pos2)
          where
            posSorted = centerBB $ last sorted
            pos1      = centerBB bb1
            pos2      = centerBB bb2

--------------------------------------------------------------------------------

generateSpawn :: [[Tile]] -> BoundingBox
generateSpawn tiles = foldl mergeBB (head boxes) (tail boxes)
  where
    boxes              = map tilePosToBB spawnTilePositions
    spawnTilePositions = map (tilePos tiles) spawnIndexes
    spawnIndexes       = map fst spawnTiles
    spawnTiles         = filter (matchTiles Spawn) (indexTiles tiles)

--------------------------------------------------------------------------------

setCreepDirection :: [WayPoint] -> Creep -> Creep
setCreepDirection waypoints c
    | (targetWayPoint c <= 1)                  = c
    | (targetWayPoint c) >= (length waypoints) = c
    | otherwise                                = c { creepRB = newRB }
      where
        newRB       = (creepRB c) { rigidVel = newCreepVel }
        newCreepVel = (newVelX, newVelY)
        newVelX     = (speed c) * (fst dir)
        newVelY     = (speed c) * (snd dir)
        dir         = direction (centerBB $ waypoints !! prevWpId) (centerBB $ waypoints !! nextWpId)
        nextWpId    = (targetWayPoint c)
        prevWpId    = nextWpId - 1
