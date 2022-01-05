module Scratch where

import Data.Metrology
import Data.Metrology.SI

maxEmission :: Temperature -> Length
maxEmission temperature = redim (x |/| temperature)
  where
    x = 2897.0 % (micro Meter :* Kelvin)


