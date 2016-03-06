{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module Tuura.Buzz (
    Time, Signal, Event, Clock, time, signal, never, once, onceAt, tick, clock,
    buzz, previous, next, lookahead, generate,
    dropRepetitions, detectRepetitions, sampler, delay, synchronise, latch
    ) where

import Control.Monad
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

data Event a = Event { timestamp :: Time, value :: a }
    deriving Functor

newtype Stream a = Stream { unstream :: [Event a] }
    deriving Functor

type Tick  = Event ()
type Clock = Stream ()

never :: Stream a
never = Stream []

onceAt :: Time -> a -> Stream a
onceAt t = Stream . return . Event t

once :: a -> Stream a
once = onceAt 0

tick :: Time -> Tick
tick t = Event t ()

generate :: [a] -> Stream a
generate = Stream . zipWith Event [0..]

buzz :: Show a => Stream a -> IO ()
buzz (Stream s) = void $ traverse putStrLn
    [ showFFloatAlt (Just 2) t ": " ++ show a | Event t a <- take 10 s ]

clock :: Time -> Clock
clock period = linearTimeScale period . Stream $ map tick [0..]

sampler :: Clock -> Signal a -> Stream a
sampler (Stream c) s = Stream [ Event t (sample s t) | Event t _ <- c ]

delay :: Time -> Stream a -> Stream a
delay delta = Stream . map (\(Event t a) -> Event (t + delta) a) . unstream

-- TODO: add exponential/hyperbolic time scaling?
linearTimeScale :: Time -> Stream a -> Stream a
linearTimeScale k = Stream . map (\(Event t a) -> Event (t * k) a) . unstream

synchronise :: Stream a -> Stream b -> Stream (a, b)
synchronise (Stream s1) (Stream s2) = Stream $ zipWith sync s1 s2
    where sync (Event ta a) (Event tb b) = Event (max ta tb) (a, b)

-- TODO: bundle events

lookahead :: Int -> Stream a -> Stream (Stream a)
lookahead n (Stream s) =
    Stream . zipWith Event (map timestamp s) $ map (Stream . take n) $ tails s

previous :: Stream a -> Stream (Maybe a)
previous (Stream s) = Stream $ Event 0.0 Nothing : map (fmap Just) s

next :: Stream a -> Stream a
next = Stream . drop 1 . unstream

dropRepetitions :: Eq a => Stream a -> Stream a
dropRepetitions s = do
    (prev, cur) <- synchronise (previous s) s
    if prev == Just cur then never else once cur

-- dropRepetitions represents waste of energy at the event source.
-- Consider sending a clock-like feedback to the source about dropped values.
detectRepetitions :: Eq a => Stream a -> Clock
detectRepetitions s = do
    (prev, cur) <- synchronise (previous s) s
    if prev == Just cur then once () else never

-- instance Foldable Stream where
--     foldr f z = foldr (f . snd) z

-- TODO: handle infinite streams efficiently
instance Monoid (Stream a) where
    mempty                        = never
    mappend (Stream x) (Stream y) = Stream $ mergeBy (comparing timestamp) x y

instance Applicative Stream where
    pure  = return
    (<*>) = ap

instance Monad Stream where
    return         = once
    Stream s >>= f = mconcat [ delay t (f a) | Event t a <- s ]

-- TODO: simplify
latch :: a -> Stream a -> Signal a
latch initial (Stream [])                           = pure initial
latch initial (Stream (Event first value : future)) = Signal $ \t ->
    if t < first then initial else sample (latch value $ Stream future) t

type Frequency = Double
type Voltage   = Double

-- Design specific mapping
type DVFS = Voltage -> Frequency

-- TODO: implement adaptiveClock
-- Adapt clock frequency to the current voltage
adaptiveClock :: DVFS -> Event Voltage -> Clock
adaptiveClock = undefined
