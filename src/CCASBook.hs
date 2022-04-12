{-# language FlexibleContexts #-}
{-# language DataKinds        #-}
{-# language TypeOperators    #-}
{-# language GADTs            #-}

-- From the book "Introduction to Carbon Capture and Sequestration"
module CCASBook where

import Types
import Constants

import Data.Metrology
import Data.Metrology.SI
import Data.Text.Chart (plot)
import System.Console.Ansigraph (graph, animate)

-- | Carnot efficiency for hot and cool temperatures.
carnotEfficiency :: Temperature -- | Hot
                 -> Temperature -- | Cool
                 -> U Number Double
carnotEfficiency hot cool = (hot |-| cool) |/| hot


-- Section 4, Question 1. (p 159)
ceCoalFiredPlant = carnotEfficiency hot cool
  where
    hot  = fromCelsius $ (550 + 600) / 2
    cool = fromCelsius 40
    -- => φ> ceCoalFiredPlant
    --    0.6307846489418145
    -- => (b)
    --
    -- (alt.) 0.75 / 0.44 = ~0.6


