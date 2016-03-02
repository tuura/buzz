{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
module Tuura.Buzz (
    Time, Signal, Event, Clock, time, signal, never, once, onceAt, buzz, clock,
    dropRepetitions, sampleWith, delay, synchronise, latch
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

newtype Event a = Event { stream :: [(Time, a)] }
    deriving (Show, Functor)

type Clock = Event ()

onceAt :: Time -> a -> Event a
onceAt t a = Event [(t, a)]

never :: Event a
never = Event []

once :: a -> Event a
once = onceAt 0

generate :: [a] -> Event a
generate = Event . zip [0..]

times :: Event a -> [Time]
times = map fst . stream

values :: Event a -> [a]
values = map snd . stream

buzz :: Show a => Event a -> IO ()
buzz e = void $ traverse putStrLn
    [ showFFloatAlt (Just 2) t ": " ++ show a | (t, a) <- take 10 $ stream e ]

clock :: Time -> Clock
clock period = Event [ (k * period, ()) | k <- [0..] ]

sampleWith :: Clock -> Signal a -> Event a
sampleWith e s = Event [ (t, sample s t) | (t, _) <- stream e ]

delay :: Time -> Event a -> Event a
delay delta e = Event [ (t + delta, a) | (t, a) <- stream e ]

synchronise :: Event a -> Event b -> Event (a, b)
synchronise ea eb = Event $ zipWith sync (stream ea) (stream eb)
    where sync (ta, a) (tb, b) = (max ta tb, (a, b))

-- TODO: bundle events

lookahead :: Int -> Event a -> Event (Event a)
lookahead n e = Event . zip (times e) . map (Event . take n) . tails $ stream e

previous :: Event a -> Event (Maybe a)
previous e = Event $ (0, Nothing) : stream (fmap Just e)

dropRepetitions :: Eq a => Event a -> Event a
dropRepetitions e = do
    (prev, cur) <- synchronise (previous e) e
    if prev == Just cur then never else once cur

instance Foldable Event where
    foldr f z e = foldr f z . map snd $ stream e

instance Monoid (Event a) where
    mempty      = Event []
    mappend x y = Event $ mergeBy (comparing fst) (stream x) (stream y)
    mconcat     = Event . monotonicMerge . map stream

monotonicMerge :: [[(Time, a)]] -> [(Time, a)]
monotonicMerge []               = []
monotonicMerge ([]:rest)        = monotonicMerge rest
monotonicMerge xs@(((t,_):_):_) = concat equal ++ monotonicMerge notEqual
  where
    (equal, notEqual) = collect xs
    collect []        = ([], [])
    collect ([]:rest) = collect rest
    collect (first@((t1,_):_):rest)
        | t == t1 = let (s , r ) = collect rest
                        (s', r') = span ((== t) . fst) first
                    in (s' : s, r' : r)
        | otherwise = ([], rest)

instance Applicative Event where
    pure  = return
    (<*>) = ap

instance Monad Event where
    return  = once
    e >>= f = mconcat [ delay t $ f a | (t, a) <- stream e ]

latch :: a -> Event a -> Signal a
latch initial (Event [])                        = pure initial
latch initial (Event ((first, value) : future)) = Signal $ \t ->
    if t < first then initial else sample (latch value (Event future)) t

-- TODO: slowdown signals/events (linear, exponential, hyperbolic)?
