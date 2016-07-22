module Datas where

import Types
import HGE2D.Types
import HGE2D.Datas


--------------------------------------------------------------------------------

data GameState = GameState
    { gsSize        :: (Double, Double)   -- current size of the window
    , time          :: Millisecond        -- current time of the game
    , level         :: Int                -- current level
    , lifes         :: Int                -- number of lifes the player has
    , gold          :: Double             -- current gold the user owns
    , world         :: World              -- the map and world data
    , buildPos      :: Maybe TilePosition -- position where maybe the build menu shall be displayed
    , hoveredTile   :: Maybe TilePosition -- position of the currently hovered tile
    , towers        :: [Tower]            -- all currently placed towers
    , creeps        :: [Creep]            -- all alive creeps
    , shots         :: [Shot]             -- all shots in the game
    , idGuiTow      :: Int                -- id of the currently selected gui tower
    , guiTowers     :: [Tower]            -- towers visible in the GUI
    }


data World = World
    { spawnPoint        :: TilePosition  -- where creeps spawn
    , wayPoints         :: [WayPoint]    -- waypoints the creeps will follow
    , tileData          :: [[Tile]]      -- the real map data
    }

--------------------------------------------------------------------------------

data Tower = Tower
    { towerPos          :: TilePosition     -- position of the tower on the map
    , towerHighlighted  :: Bool             -- whether the tower is highlighted
    , base              :: Base             -- stability
    , center            :: Center           -- height & fire range
    , gun               :: Gun              -- pew pew
    , radar             :: Radar            -- tracking enemies, range, precision of position and speed
    , creepSelector     :: CreepSelector    -- selects the creep to aim at from a given list of creeps
    , aimController     :: AimController    -- decides how to aim at a creep
    , rangeFilter       :: RangeFilter      -- filters found enemies by range
    } deriving (Show, Read)


data CreepSelector = SelectFirst        -- simply selecting the very first creep
                   | SelectClosest      -- selecting the closest creep
                   | SelectLowestHp     -- selecting creep with lowest hp
                   | SelectHighestHp    -- selecting creep with highest hp
                   | SelectFastest      -- selecting the creep which travels the fastest
                   | SelectSlowest      -- selecting the creep which travels the slowest
                   | SelectMostSlowed   -- selecting the creep where slow time is the largest
                   | SelectLeastSlowed  -- selecting the creep where slow time is the smallest
                   | SelectMostBurning  -- selecting the creep where burn time is the largest
                   | SelectLeastBurning -- selecting the creep where burn time is the smallest
                   deriving (Show, Read)


data AimController = AimAt00            -- aiming at position 0 0 (TODO remove later for debugging)
                   | AimDirectly        -- directly aiming at the creep
                   | AimAtIntersection  -- aiming at the point where creep and shot will meet
                   deriving (Show, Read)


data RangeFilter = RangeFilter
    { filterRange :: Range -- range which is used for filtering
    } deriving (Show, Read)


data Base = Base1
          | Base2
          | Base3
          deriving (Show, Read)


data Center = Center1
            | Center2
            | Center3
            deriving (Show, Read)


data Radar = Radar
    { radarSkin :: RadarSkin    -- skin for the radar which defines how to render it
    , scanRange :: Range       -- range in which enemies are visible
    , anglRange :: Radian       -- angle in which enemies are visible
    , scanRate  :: Double       -- updates per second
    } deriving (Show, Read)


data RadarSkin = RadarSkin1
               deriving (Show, Read)

data Gun = Gun
    { gunSkin    :: GunSkin     -- skin of the gun which defines how to render it
    , reloadTime :: Millisecond -- time until next shot can be fired
    , fireRate   :: Double      -- shots per second
    , shot       :: Shot        -- shot fired by this gun
    , radGun     :: Radian      -- rotation of the gun in radians
    , gunEnabled :: Bool        -- whether this gun will shoot when possible
    } deriving (Show, Read)


data GunSkin = GunSkinCannon
             | GunSkinLaser
             | GunSkinSlow
             deriving (Show, Read)

--------------------------------------------------------------------------------

data Shot = Shot
    { shotSkin   :: ShotSkin        -- the skin of the shot defining its draw style
    , shotStart  :: RealPosition    -- position from which this shot was fired
    , shotRB     :: RigidBody       -- position, velocity and bounding box
    , shotDamage :: Damage          -- damage dealt on hit
    , shotRange  :: Range           -- range of the shot
    , shotSpeed  :: Double          -- speed of the shot
    , splash     :: Range           -- splash range dealing damage
    , dps        :: Dps             -- burn damage after hit ---TODO rename to burn
    , dpsDur     :: Millisecond     -- time the target will burn ---TODO rename properly tower has basically same field
    , slow       :: Slow            -- whether enemies are slowed by shots
    , slowDur    :: Millisecond     -- time the target will be slowed --- TODO rename properly
    , hitsAir    :: Bool            -- whether the gun can hit air units
    , hitsMany   :: Bool            -- whether shots are consumed on first hit or can hit multiple targets ---TODO rename to penetrates or something (or make this a penetrationfactor)
    } deriving (Show, Read)


data ShotSkin = ShotSkinCannon
              | ShotSkinLaser
              | ShotSkinSlow
              deriving (Show, Read)

--------------------------------------------------------------------------------

data Creep = Creep
    { creepSkin      :: CreepSkin   -- skin of the creep
    , creepRB        :: RigidBody   -- position velocity and bounding box
    , initHp         :: Hp          -- initial hitpoints / life
    , hp             :: Hp          -- hitpoints / life
    , speed          :: Double      -- walk speed
    , creepValue     :: Double      -- value of the creep
    , slowFactor     :: Slow        -- how much this creep is slowed
    , slowDuration   :: Millisecond -- time until slow wears off
    , burnAmount     :: Dps         -- how much this creep is burned
    , burnDuration   :: Millisecond -- time until burn wears off
    , targetWayPoint :: Int         -- the id of the waypoint this creep wants to reach
    , canFly         :: Bool        -- whether the enemy can fly
    } deriving (Show, Read)


data CreepSkin = CreepSkin1 deriving (Show, Read)

--------------------------------------------------------------------------------

data Tile = Water
          | Grass
          | Rock
          | Forest
          | Path
          | Spawn
          deriving (Show, Eq)

