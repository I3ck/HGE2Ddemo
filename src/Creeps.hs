module Creeps where

import Datas
import Factory

import HGE2D.Datas
import HGE2D.Geometry

creep1 :: Creep
creep1 = makeCreep CreepSkin1
                   (makeRB (50, 420) (0.03, 0) 10 10)
                   5000
                   0.03
                   25
                   False
