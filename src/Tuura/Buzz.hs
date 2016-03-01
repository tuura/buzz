{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
module Tuura.Buzz where

import Data.List.Extra
import Data.Ord

type Time = Double

newtype Signal a = Signal { sample :: Time -> a }
    deriving Functor

instance Applicative Signal where
    pure                  = Signal . const
    Signal f <*> Signal g = Signal $ \t -> (f t) (g t)

time :: Signal Time
time = Signal id

newtype Event a = Event { stream :: [(Time, a)] }
    deriving (Show, Functor)

type Clock = Event ()

never :: Event a
never = Event []

event :: Time -> a -> Event a
event t a = Event [(t, a)]

clock :: Time -> Clock
clock period = Event [ (k * period, ()) | k <- [0..] ]

toClock :: Event a -> Clock
toClock = fmap $ const ()

sampleWith :: Clock -> Signal a -> Event a
sampleWith Event {..} Signal {..} = Event [ (t, sample t) | (t, _) <- stream ]

instance Monoid (Event a) where
    mempty      = never
    mappend x y = Event $ mergeBy (comparing fst) (stream x) (stream y)

latch :: a -> Event a -> Signal a
latch initial (Event [])                        = pure initial
latch initial (Event ((first, value) : future)) = Signal $ \t ->
    if t < first then initial else sample (latch value (Event future)) t

-- TODO: slowdown signals/events (linear, exponential, hyperbolic)?
