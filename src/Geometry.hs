module Geometry where

import Settings

import HGE2D.Types
import HGE2D.Datas

toTilePos :: RealPosition -> TilePosition
toTilePos real = TilePosition { tileX = newX, tileY = newY}
  where
    newX = (round $ fst real) `quot` (round tileSize)
    newY = (round $ snd real) `quot` (round tileSize)


toRealPos :: TilePosition -> RealPosition
toRealPos tile = (newX, newY)
  where
      newX = tileSize * (fromIntegral $ tileX tile)
      newY = tileSize * (fromIntegral $ tileY tile)


toRealPosCentered :: TilePosition -> RealPosition
toRealPosCentered tile = (newX, newY)
  where
      newX = topleftX + tileSize / 2.0
      newY = topleftY + tileSize / 2.0
      topleftX = fst $ toRealPos tile
      topleftY = snd $ toRealPos tile

tilePosToBB :: TilePosition -> BoundingBox
tilePosToBB pos = BoundingBox minPos maxPos
  where
    minPos = toRealPos $ pos
    maxPos = (maxX, maxY)
    maxX = (fst minPos) + tileSize
    maxY = (snd minPos) + tileSize
