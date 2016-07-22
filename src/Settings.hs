module Settings where

import HGE2D.Types

copyright :: String
copyright = "(c) Martin Buck"

title :: String
title = "HGE2Ddemo " ++ copyright

--------------------------------------------------------------------------------

tileSize :: Pixel
tileSize = 50

mapTilesX :: Int
mapTilesX = 28

mapTilesY :: Int
mapTilesY = 15

mapSizeX :: Pixel
mapSizeX = fromIntegral mapTilesX * tileSize

mapSizeY :: Pixel
mapSizeY = fromIntegral mapTilesY * tileSize

sideBarWidth :: Pixel
sideBarWidth = 300

widthHpBar :: Pixel
widthHpBar = tileSize / 2

heightHpBar :: Pixel
heightHpBar = widthHpBar / 3

creepSpawnMargin :: Pixel
creepSpawnMargin = 5

--------------------------------------------------------------------------------

windowTitle :: String
windowTitle = "HGE2Ddemo"

windowWidth :: Double
windowWidth = totalSizeX

windowHeight :: Double
windowHeight = totalSizeY

nFacesCircle :: Int
nFacesCircle = 100

--------------------------------------------------------------------------------

totalSizeX :: Pixel
totalSizeX = mapSizeX + sideBarWidth

totalSizeY :: Pixel
totalSizeY = mapSizeY
