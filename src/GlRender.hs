module GlRender where

import Settings
import Types
import Datas
import Geometry
import Valuable
import HigherOrder
import GlColors

import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Geometry
import HGE2D.Render (withCamera)
import HGE2D.Shapes
import HGE2D.GlFunctions

import Graphics.UI.GLUT

rtf = realToFrac :: Double -> GLfloat
fri = fromIntegral :: Int -> Double

renderGs :: EngineState GameState -> GameState -> RenderInstruction
renderGs es gs
        | lifes gs <= 0 = lostText
        | (length $ creeps gs) <= 0 = wonText
        | otherwise = withCamera es gs $ RenderMany
          [ toGlInstruction (world gs)
          , renderHoveredTile
          , RenderMany (map toGlInstruction (towers gs))
          , RenderMany (map toGlInstruction (creeps gs))
          , RenderMany (map toGlInstruction (shots gs))
          , renderInfo
          , renderSideBar
         -- , renderBuildRing ---TODO only when in build mode
          ]
      where
        lostText = RenderMany [ RenderColorize colorRed, RenderText "You lost" ]
        wonText  = RenderMany [ RenderColorize colorGreen, RenderText "You won" ]

        renderHoveredTile = case (hoveredTile gs) of
           Nothing     -> RenderNothing
           (Just ht)   -> RenderPreserve $ RenderMany
               [ RenderTranslate (rtf $ getX ht) (rtf $ getY ht)
               , RenderColorizeAlpha  (addAlpha 0.2 colorSelected)
               , rectangle (realToFrac tileSize) (realToFrac tileSize)
               ]

        renderInfo = RenderMany
            [ RenderPreserve $ RenderMany
                [ RenderTranslate 65 75
                , borderedRectangle 130 150 1 colorBlack (addAlpha 1.0 colorWhite )
                ]
            , RenderColorize colorWhite
            , RenderPreserve $ RenderMany [RenderTranslate 20 20,  text ("Gold :   " ++ (show $ gold gs)) ]
            , RenderPreserve $ RenderMany [RenderTranslate 20 40,  text ("Level :  " ++ (show $ level gs)) ]
            , RenderPreserve $ RenderMany [RenderTranslate 20 60,  text ("Lifes :  " ++ (show $ lifes gs )) ]
            , RenderPreserve $ RenderMany [RenderTranslate 20 80,  text ("Towers : " ++ (show $ length $ towers gs)) ]
            , RenderPreserve $ RenderMany [RenderTranslate 20 100, text ("Shots :  " ++ (show $ length $ shots gs)) ]
            , RenderPreserve $ RenderMany [RenderTranslate 20 120, text ("Creeps : " ++ (show $ length $ creeps gs)) ]
            ] ---TODO different colors for the values (gold golden, lifes green, level black?)

        renderBuildRing = case (buildPos gs) of
            Nothing     -> RenderNothing
            (Just bp)   -> RenderMany
                [ RenderPreserve $ RenderMany
                    [ RenderTranslate (rtf $ getX bp) (rtf $ getY bp)
                    , ring 0.3 10 ---TODO translate to bp
                    ]
                , RenderMany $ map towersCircle (zip [0..] positionedTowers)
                ]
              where
                positionedTowers = map moveToTile (guiTowers gs)
                  where
                    moveToTile :: Tower -> Tower
                    moveToTile t = t { towerPos = bp } ---TODO this must be cursorpos

                towersCircle :: (Int, Tower) -> RenderInstruction
                towersCircle (i, t) = RenderPreserve $ RenderMany
                  [ RenderTranslate 0 (rtf $ fri $ i `quot` 5) ---TODO must be in a circle
                  , toGlInstruction t ---TODO this must re-calculate position, guitowers shall all be centered
                  ]

        renderSideBar = RenderMany
            [ RenderPreserve $ RenderMany
                [ RenderTranslate (rtf $ mapSizeX + sideBarWidth / 2) 0
                , borderedRectangle (rtf sideBarWidth) (rtf totalSizeY) 3 colorSideBar (addAlpha 1.0 colorBlack )
                ]
            , RenderMany $ map toGlInstruction (guiTowers gs)
            , selectionBox
            ,  towerInfo
            ]

          where
            selectionBox = RenderPreserve $ RenderMany
                 [ RenderColorizeAlpha  (addAlpha 0.3 colorSelected)
                 , RenderTranslate
                     (rtf $ mapSizeX + tileSize / 2)
                     (rtf $ tileSize * (fromIntegral $ idGuiTow gs) + tileSize / 2)
                 , rectangle (rtf tileSize) (rtf tileSize)
                 ]

            towerInfo    =  RenderMany
                                [ RenderColorize colorBlack
                                , RenderMany (map renderInfo (guiTowers gs))
                                ]

            renderInfo :: Tower -> RenderInstruction
            renderInfo t = RenderPreserve $ RenderMany
                [ RenderTranslate (rtf textX) (rtf textY)
                , RenderText ((show $ valueBuy t) ++ " g")
                ]
              where
                textX   = tileSize + fst textPos
                textY   =            snd textPos
                textPos = toRealPosCentered $ towerPos t

instance GlInstructable Creep where
    toGlInstruction creep = RenderMany [creepRender $ creepSkin creep, hpBar]
      where
        hpBar = RenderPreserve $ RenderMany
          [ RenderTranslate x hpY
          , RenderColorizeAlpha (addAlpha 0.5 colorRed)
          , rectangle (rtf widthHpBar) (rtf heightHpBar)
          , RenderColorizeAlpha (addAlpha 0.5 colorGreen)
          , rectangle (rtf widthGreen) (rtf heightHpBar)
          ]

        hpY = y - (1.25 * r)
        widthGreen = hpRatio * widthHpBar
        hpRatio = (hp creep) / (initHp creep)

        creepRender :: CreepSkin -> RenderInstruction --- TODO specialise for skins
        creepRender _ = RenderPreserve $ RenderMany
          [ RenderTranslate x y
          , RenderColorize colorWhite
          , circle r
          ]

        x = rtf $ getX creep
        y = rtf $ getY creep
        r = rtf $ tileSize / 4


instance GlInstructable Shot where
    toGlInstruction shot = RenderPreserve $ RenderMany [RenderTranslate (rtf x) (rtf y), RenderRotate angle, shotRender $ shotSkin shot]
      where
        shotRender :: ShotSkin -> RenderInstruction
        shotRender ShotSkinCannon =
            RenderMany [RenderColorize colorBlack, circle 4]
        shotRender ShotSkinLaser  =
            RenderMany [RenderColorizeAlpha (addAlpha 0.5 colorLaser), rectangle 15 2]
        shotRender ShotSkinSlow   =
            RenderMany [RenderColorize colorIce, circle 2]

        x       = getX shot
        y       = getY shot
        angle   = velAngle $ rigidVel $ shotRB shot

instance GlInstructable Tower where
    toGlInstruction t = RenderMany
      [ baseRender (base t)
      , centerRender (center t)
      , RenderPreserve $ RenderMany
        [ RenderTranslate (rtf centerX) (rtf centerY)
        , RenderRotate (radGun $ gun t)
        , gunSkinRender (gunSkin $ gun t)
        , radarRender (radarSkin $ radar t)
        , rangeRenders
        ]
      ]
      where
          centerX = getX t
          centerY = getY t

          rangeRenders | not (towerHighlighted t) = RenderNothing
                       | otherwise = RenderMany [shotRangeRender (shotRange $ shot $ gun t), radarRangeRender (scanRange $ radar t)]

          baseRender :: Base -> RenderInstruction
          baseRender Base1 = RenderMany [RenderColorize colorMetal, foot1, foot2, foot3, bottom]
            where
              foot1 = RenderPreserve $ RenderMany [RenderTranslate (fst centerFoot1) (snd centerFoot1), circle  (rtf $ tileSize / 6)]
              foot2 = RenderPreserve $ RenderMany [RenderTranslate (fst centerFoot2) (snd centerFoot2), circle  (rtf $ tileSize / 6)]
              foot3 = RenderPreserve $ RenderMany [RenderTranslate (fst centerFoot3) (snd centerFoot3), circle  (rtf $ tileSize / 6)]
              bottom = RenderPreserve $ RenderMany [RenderTranslate (rtf centerX) (rtf centerY), rectangle (rtf $ tileSize / 2) (rtf $ tileSize / 2)]

              centerFoot1 = ((rtf $ centerX - tileSize / 4), (rtf $ centerY + tileSize / 4))
              centerFoot2 = ((rtf $ centerX + tileSize / 4), (rtf $ centerY + tileSize / 4))
              centerFoot3 = (rtf $ centerX, (rtf $ centerY - tileSize / 4))
          baseRender Base2 = RenderNothing ---TODO
          baseRender Base3 = RenderNothing ---TODO

          centerRender :: Center -> RenderInstruction
          centerRender Center1 = RenderMany [RenderColorize colorBlack, tower]
            where
              tower = RenderPreserve $ RenderMany [RenderTranslate (rtf centerX) (rtf centerY), circle (rtf $ tileSize / 8)]
          centerRender Center2 = RenderNothing ---TODO
          centerRender Center3 = RenderNothing ---TODO

          gunSkinRender :: GunSkin -> RenderInstruction ---TODO currently no distinct skins
          gunSkinRender GunSkinLaser = RenderMany [RenderColorize colorBlack, centerPiece, RenderColorize colorBlack, barrell]
            where
              barrell = line (point2 0 0) (point2 (rtf $ tileSize / 4) 0) 5
              centerPiece = rectangle (rtf sizeX) (rtf sizeY)
              sizeX = tileSize / 8
              sizeY = tileSize / 4

          gunSkinRender GunSkinCannon = RenderMany [RenderColorize colorIce, centerPiece, RenderColorize colorBlack, barrell]
            where
              barrell = line (point2 0 0) (point2 (rtf $ tileSize / 4) 0) 5
              centerPiece = rectangle (rtf sizeX) (rtf sizeY)
              sizeX = tileSize / 4
              sizeY = tileSize / 2

          gunSkinRender GunSkinSlow = RenderMany [RenderColorize colorIce, centerPiece, RenderColorize colorWhite, barrell]
            where
              barrell = line (point2 0 0) (point2 (rtf $ tileSize / 4) 0) 5
              centerPiece = rectangle (rtf sizeX) (rtf sizeY)
              sizeX = tileSize / 8
              sizeY = tileSize / 4

          shotRangeRender :: Range -> RenderInstruction
          shotRangeRender r = RenderMany [RenderColorizeAlpha (addAlpha 0.1 colorShotRange), circle (rtf r)]

          radarRangeRender :: Range -> RenderInstruction
          radarRangeRender r = RenderMany [RenderColorizeAlpha (addAlpha 0.1 colorRadarRange), circle (rtf r)]

          radarRender :: RadarSkin -> RenderInstruction
          radarRender RadarSkin1 = RenderMany [antenna1, antenna2]
            where
              antenna1 = RenderMany [RenderColorize colorBlack, line (point2 0 (rtf $   tileSize / 8)) (point2 (rtf $ - tileSize / 3) (rtf $   tileSize / 8)) 1]
              antenna2 = RenderMany [RenderColorize colorBlack, line (point2 0 (rtf $ - tileSize / 8)) (point2 (rtf $ - tileSize / 3) (rtf $ - tileSize / 8)) 1]


instance GlInstructable World where
    toGlInstruction w = RenderMany (map glTile (indexTiles tiles))
      where
        tiles = tileData w

        glTile :: (Int, Tile) -> RenderInstruction
        glTile (index, Water)   = rectByIndex index colorWater
        glTile (index, Grass)   = rectByIndex index colorGrass
        glTile (index, Rock)    = rectByIndex index colorRock
        glTile (index, Forest)  = rectByIndex index colorForest
        glTile (index, Path)    = rectByIndex index colorPath
        glTile (index, Spawn)   = rectByIndex index colorWhite

        rectByIndex :: Int -> (GLfloat, GLfloat, GLfloat) -> RenderInstruction
        rectByIndex index innerColor = RenderPreserve $ RenderMany [RenderTranslate tlPosX tlPosY, borderedRectangle (rtf tileSize) (rtf tileSize) 1 innerColor (addAlpha 0.1 colorGrid)]
          where
            tlPosX = rtf $ fst (toRealPosCentered $ tilePos tiles index)
            tlPosY = rtf $ snd (toRealPosCentered $ tilePos tiles index)
