module Chapter3 where

import Data.Metrology
import Data.Metrology.SI

-- Note: Sad hack. See: <https://github.com/goldfirere/units/issues/39>
fromCelsius :: Double -> Temperature
fromCelsius x = (x + 273.15) % Kelvin

(c⁰) = fromCelsius

toCelsius :: Temperature -> Double
toCelsius k = (k # Kelvin) - 273.15


{-
  Question 1.

  On a winter night, a person sits next to a window with an effective
  temperature of 6⁰C.

  a) How much longwave radiation does the window emit?
  
  With a closed curtain the effective temperature is 18⁰C.

  b) How much longwave radiation is emitted? Assume an emisivity of one.
  c) Discuss why the person feels warmer with the curtain closed.
-}
q1_a = undefined
  where
    temp        = c⁰ 6
    curtainTemp = c⁰ 18
