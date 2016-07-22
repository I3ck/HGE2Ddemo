module Guns where

import Datas
import Shots
import Factory

gunCannon :: Gun
gunCannon = makeGun GunSkinCannon 0.0005 shotCannon

gunLaser :: Gun
gunLaser = makeGun GunSkinLaser 0.005 shotLaser

gunSlow :: Gun
gunSlow = makeGun GunSkinSlow 0.003 shotSlow
