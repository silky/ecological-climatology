{-# language FlexibleContexts #-}
{-# language DataKinds        #-}
{-# language TypeOperators    #-}
{-# language GADTs    #-}

module Chapter3 where

import Types
import Constants

import Data.Metrology
import Data.Metrology.SI

-- | See: <https://en.wikipedia.org/wiki/Planck%27s_law>
plancksLaw :: Length
           -> Temperature
           -> U' (Watt :/ (Meter :^ Three))
plancksLaw wavelength temperature = redim $ a |/| b
  where
    a :: U' (Joule :* (Meter :^ Two) :/ Second)
    a = tau |*| (planckConstant |*| (speedOfLight |^ sTwo))

    t :: U' Number
    t = (planckConstant |*| speedOfLight)
          |/| (wavelength |*| boltzmannConstant |*| temperature)

    b :: U' (Meter :^ Five)
    b = (wavelength |^ sFive) |*| (exp t - 1)


{-
  Question 1.

  On a winter night, a person sits next to a window with an effective
  temperature of 6⁰C.

  a) How much longwave radiation does the window emit?

  With a closed curtain the effective temperature is 18⁰C.

  b) How much longwave radiation is emitted? Assume an emisivity of one.
  c) Discuss why the person feels warmer with the curtain closed.
-}
q1_a = map (s . f) wavelengths
  where
    f w = plancksLaw (w % micro Meter) temp
    wavelengths = [ 4 .. 50 ]
    -- wavelengths = [ l * 1/10 | l <- [1 .. 10] ]
    --                 ++ [ 2 .. 10 ]
    --                 ++ [ l * 10 | l <- [2 .. 10] ]

    -- Note: Maybe it's better to use these units instead of m^-3,
    -- as it gives numbers that are a bit more readily consumed.
    s x  = redim x # (Watt :/ (Meter :^ sTwo) :/ micro Meter)
    -- temp = c⁰ 6
    temp = c⁰ 18

-- Notes:
--
--    > Google says "longwave" radiation is 4-30 μm.

-- a) Answer: ...










-- Notes:
-- 1. Bug; it doesn't associate well.
--
-- Bad:
--  a = 2 *| pi *| planckConstant |*| (speedOfLight |^ sTwo)
--
-- Good:
--  a = 2 *| (pi *| (planckConstant |*| (speedOfLight |^ sTwo)))

-- 2. Bug; Steradians in units causing an issue.
--
--  Bad:  plancksLaw :: U' (Watt :/ (Meter :^ Three) :/ Steradian)
--  Good: plancksLaw :: U' (Watt :/ (Meter :^ Three))
