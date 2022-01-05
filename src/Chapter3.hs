{-# language FlexibleContexts    #-}
{-# language DataKinds           #-}
{-# language AllowAmbiguousTypes #-}

module Chapter3 where

{-

Using `units`

import Data.Metrology
import Data.Metrology.SI
-- import Data.Metrology.SI.Poly

-- Note: Sad hack. See: <https://github.com/goldfirere/units/issues/39>
fromCelsius :: Double -> Temperature
fromCelsius x = (x + 273.15) % Kelvin

(c⁰) = fromCelsius

toCelsius :: Temperature -> Double
toCelsius k = (k # Kelvin) - 273.15

type U unit a = MkQu_ULN unit DefaultLCSU a

-- ~~ Constants
planckConstant :: Floating a => U (Joule :* Second) a
planckConstant = (6.6260695729e-34) % (Joule :* Second)

speedOfLight :: Floating a => U (Meter :/ Second) a
speedOfLight = 3e8 % (Meter :/ Second)

boltzmannConstant :: Floating a => U (Joule :/ Kelvin) a
boltzmannConstant = (1.38e-23) % (Joule :/ Kelvin)

-- TODO: Why can't `Meter` be `Length`?
-- maxEmission :: Temperature -> U Meter Double
maxEmission :: Temperature -> U Meter Double
maxEmission temperature = redim (x |/| temperature)
  where
    x = 2897.0 % (micro Meter :* Kelvin)


-- wavelength = Λ
-- plancksLaw ::
-- plancksLaw :: Meter -> Temperature -> U (Eener
-- plancksLaw wavelength temperature = a |/| b
--   where
--     -- a = 2 |* pi |* planckConstant |*|
--     a = 2 *| pi *| planckConstant |*| (speedOfLight |^ sTwo)
--     t = (planckConstant |*| speedOfLight) |/| (wavelength |*| boltzmannConstant |*| temperature)
--     b = 1
    -- w = wavelength :^ 5
    -- t = planckConstant |* speedOfLight :/ (boltzmannConstant :* wavelength :* temperature)
    -- b = undefined
    -- b = w * exp\\\

-}

import Prelude hiding ((*), (/), (^), exp, pi)
import Numeric.Units.Dimensional.Prelude

type AngularMomentum' = AngularMomentum Double
type Velocity'        = Velocity Double
type HeatCapacity'    = HeatCapacity Double
type Celsius'         = CelsiusTemperature Double
type Length'          = Length Double

planckConstant :: AngularMomentum'
planckConstant = 6.62e-34 *~ (joule * second)

speedOfLight :: Velocity'
speedOfLight = 3e8 *~ (meter / second)

boltzmannConstant :: HeatCapacity'
boltzmannConstant = 1.38e-23 *~ (joule / kelvin)

maxEmission :: Celsius' -> Length'
maxEmission temperature = x / temperature
  where
    x = 2897.0 *~ (micro meter * kelvin)

-- plancksLaw :: Length' -> Celsius' -> _
plancksLaw = a / b
  where
    -- wavelength :: Length'
    wavelength = 1 *~ micro meter
    -- temperature :: Length'
    temperature = 1 *~ kelvin
    a = 2 * pi * planckConstant * (speedOfLight ^ pos2)
    t = (planckConstant * speedOfLight) / (wavelength * boltzmannConstant * temperature)
    b = (wavelength ^ pos5) * exp t
    -- b = (wavelength ^ pos5)
    -- b = exp t
    -- b = exp _1
    -- b = 1



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
  -- where
  --   temp        = c⁰ 6
  --   curtainTemp = c⁰ 18
