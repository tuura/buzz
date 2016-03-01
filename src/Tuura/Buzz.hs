{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
module Tuura.Buzz (
    Time, Signal, Event, Clock, time, signal, never, event, buzz, clock,
    dropRepetitions, sampleWith, delay, synchronise, latch
    ) where

import Control.Monad
import Data.Function
import Data.List.Extra
import Data.Ord
import Numeric

type Time = Double

newtype Signal a = Signal { sample :: Time -> a }
    deriving Functor

instance Applicative Signal where
    pure                  = Signal . const
    Signal f <*> Signal g = Signal $ \t -> f t (g t)

time :: Signal Time
time = Signal id

signal :: (Time -> a) -> Signal a
signal = Signal

newtype Event a = Event { stream :: [(Time, a)] }
    deriving (Show, Functor)

type Clock = Event ()

never :: Event a
never = Event []

event :: Time -> a -> Event a
event t a = Event [(t, a)]

buzz :: Show a => Event a -> IO ()
buzz Event {..} = void $ traverse putStrLn
    [ showFFloatAlt (Just 2) t "" ++ ": " ++ show a | (t, a) <- take 10 stream ]

clock :: Time -> Clock
clock period = Event [ (k * period, ()) | k <- [0..] ]

sampleWith :: Clock -> Signal a -> Event a
sampleWith Event {..} Signal {..} = Event [ (t, sample t) | (t, _) <- stream ]

delay :: Time -> Event a -> Event a
delay period Event {..} = Event [ (t + period, a) | (t, a) <- stream ]

synchronise :: Event a -> Event b -> Event (a, b)
synchronise (Event as) (Event bs) = Event $ zipWith sync as bs
    where sync (ta, a) (tb, b) = (max ta tb, (a, b))

dropRepetitions :: Eq a => Event a -> Event a
dropRepetitions Event {..} = Event . map head $ groupBy ((==) `on` snd) stream

instance Foldable Event where
    foldr f z Event {..} = foldr f z $ map snd stream

instance Monoid (Event a) where
    mempty      = never
    mappend x y = Event $ mergeBy (comparing fst) (stream x) (stream y)

instance Applicative Event where
    pure  = return
    (<*>) = ap

instance Monad Event where
    return           = event 0
    Event {..} >>= f = mconcat $ map (\(t, e) -> delay t $ f e) stream

latch :: a -> Event a -> Signal a
latch initial (Event [])                        = pure initial
latch initial (Event ((first, value) : future)) = Signal $ \t ->
    if t < first then initial else sample (latch value (Event future)) t

-- TODO: slowdown signals/events (linear, exponential, hyperbolic)?
