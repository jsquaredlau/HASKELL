module Test where

import Test.QuickCheck
import World
import Physics
import Simulation
import TestSupport

prop_EnergyConservation :: World -> Bool
prop_EnergyConservation (World a b c d)
  | length d == 0 = True
  | otherwise = let p = worldEnergy (World a b c d)
                    q = worldEnergy (advanceWorld 0 0.001 (World a b c d))
                in ((abs (p-q))/p) < (realToFrac epsilon)
