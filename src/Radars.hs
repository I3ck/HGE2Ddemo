module Radars where

import Datas

radarSonar :: Radar
radarSonar = Radar
    { radarSkin = RadarSkin1
    , scanRange = 150
    , anglRange = 360
    , scanRate = 0.2
    }

radarLaser :: Radar
radarLaser = Radar
    { radarSkin = RadarSkin1
    , scanRange = 150
    , anglRange = 10
    , scanRate = 20
    }

radarSonic :: Radar
radarSonic = Radar
    { radarSkin = RadarSkin1
    , scanRange = 100
    , anglRange = 360
    , scanRate = 20
    }
