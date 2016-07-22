module Valuable where

import Datas

---TODO also instanciate creep?

class Valuable a where
    valueBuy :: a -> Double
    valueSell :: a -> Double

--------------------------------------------------------------------------------

instance Valuable Tower where
    valueBuy t = (valueBuy $ base t)
               + (valueBuy $ center t)
               + (valueBuy $ gun t)
               + (valueBuy $ radar t)
               + (valueBuy $ creepSelector t)
               + (valueBuy $ aimController t)
               + (valueBuy $ rangeFilter t)

    valueSell t = (valueSell $ base t)
                + (valueSell $ center t)
                + (valueSell $ gun t)
                + (valueSell $ radar t)
                + (valueSell $ creepSelector t)
                + (valueSell $ aimController t)
                + (valueSell $ rangeFilter t)

--------------------------------------------------------------------------------

instance Valuable Base where
    valueBuy _ = 10 ---TODO add member for this
    valueSell _ = 10 ---TODO add member for this or calculate depending on buy value

--------------------------------------------------------------------------------

instance Valuable Center where
    valueBuy _ = 10 ---TODO add member for this
    valueSell _ = 10 ---TODO add member for this or calculate depending on buy value

--------------------------------------------------------------------------------

instance Valuable Gun where
    valueBuy _ = 10 ---TODO add member for this
    valueSell _ = 10 ---TODO add member for this or calculate depending on buy value

--------------------------------------------------------------------------------

instance Valuable Radar where
    valueBuy _ = 10 ---TODO add member for this
    valueSell _ = 10 ---TODO add member for this or calculate depending on buy value

--------------------------------------------------------------------------------

instance Valuable CreepSelector where
    valueBuy _ = 10 ---TODO add member for this
    valueSell _ = 10 ---TODO add member for this or calculate depending on buy value

--------------------------------------------------------------------------------

instance Valuable AimController where
    valueBuy _ = 10 ---TODO add member for this
    valueSell _ = 10 ---TODO add member for this or calculate depending on buy value

--------------------------------------------------------------------------------

instance Valuable RangeFilter where
    valueBuy _ = 10 ---TODO add member for this
    valueSell _ = 10 ---TODO add member for this or calculate depending on buy value
