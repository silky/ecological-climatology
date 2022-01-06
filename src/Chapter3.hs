{-# language FlexibleContexts #-}
{-# language DataKinds        #-}
{-# language TypeOperators    #-}
{-# language GADTs            #-}

module Chapter3 where

import Types
import Constants

import Data.Metrology
import Data.Metrology.SI
import Data.Text.Chart (plot)
import System.Console.Ansigraph (graph, animate)

-- | See: <https://en.wikipedia.org/wiki/Planck%27s_law>
--
-- Eq. 3.1 in the book.
--
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

  b) How much longwave radiation is emitted?

  Assume an emisivity of one.

  c) Discuss why the person feels warmer with the curtain closed.
-}
q1 temp = map (s . f) wavelengths
  where
    f w = plancksLaw (w % micro Meter) temp
    wavelengths = [ 3 .. 100 ]
    -- Note: Maybe it's better to use these units instead of m^-3,
    -- as it gives numbers that are a bit more readily consumed.
    s x  = redim x # (Watt :/ (Meter :^ sTwo) :/ micro Meter)

q1a = q1 $ c⁰ 6
q1b = q1 $ c⁰ 18

-- Notes:
--
--    - The book says long-way radiation is 3-100 μm.

-- a) Answer: ...
-- b) Answer:
-- c) Answer: 










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
