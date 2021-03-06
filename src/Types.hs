{-# language DataKinds     #-}
{-# language TypeOperators #-}

module Types where

import Data.Metrology
import Data.Metrology.SI

-- | Handy representations of types.
type U unit a = MkQu_ULN unit DefaultLCSU a
type U' unit  = MkQu_ULN unit DefaultLCSU Double

-- Note: Sad hack. See: <https://github.com/goldfirere/units/issues/39>
fromCelsius :: Double -> Temperature
fromCelsius x = (x + 273.15) % Kelvin

(c⁰) = fromCelsius

toCelsius :: Temperature -> Double
toCelsius k = (k # Kelvin) - 273.15
