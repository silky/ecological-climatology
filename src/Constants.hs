module Constants where

import Types
import Data.Metrology
import Data.Metrology.SI

planckConstant :: Floating a => U (Joule :* Second) a
planckConstant = (6.6260695729e-34) % (Joule :* Second)

speedOfLight :: Floating a => U (Meter :/ Second) a
speedOfLight = 3e8 % (Meter :/ Second)

boltzmannConstant :: Floating a => U (Joule :/ Kelvin) a
boltzmannConstant = (1.38e-23) % (Joule :/ Kelvin)

tau :: U Number Double
tau  = constant $ 2 * pi
