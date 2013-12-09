{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, MultiParamTypeClasses, RankNTypes, UnboxedTuples, FunctionalDependencies, RecordWildCards, ExistentialQuantification, BangPatterns #-}

--import Prelude hiding (replicate, (++))
import Data.List hiding (replicate, (++))
import Data.Vector (Vector (..), (!), generate)
import qualified Data.Vector as V

import Text.Printf
import Control.Monad

import Graphics.EasyPlot
 
dt, dx, u, kappa :: Float
dt = 0.000001
dx = 0.1
u = 5000.0
kappa = 300.0
st, re :: Float
st = (u * dt) / dx
re = (kappa * dt) / (dx * dx)
--re = kappa * dt / dx / dx
x_left, x_right :: Float
x_left = 1.0
x_right = 0.0


time_steps, space_steps :: Int
time_steps = 200
space_steps = 200

initial :: Int -> Vector Float
initial size = V.replicate (size `div` 2) 1.0 V.++ V.replicate (size - (size `div` 2)) 0.0

type MethodType = Int -> Int -> [Vector Float]
type EulerStep = Vector Float -> Vector Float

eulerForward :: EulerStep -> MethodType
eulerForward step tn sn = take tn (iterate step (initial sn))

eulerForwardAgainstFlow, eulerForwardByFlow :: MethodType
eulerForwardByFlow = eulerForward eulerForwardByFlowStep -- #1
eulerForwardAgainstFlow = eulerForward eulerForwardAgainstFlowStep -- #2

eulerForwardStep :: (Vector Float -> Int -> Float) -> EulerStep
eulerForwardStep gen xs = let
  n = V.length xs
  generator i
    | (i == 0)     = x_left
    | (i == n - 1) = x_right
    | otherwise    = gen xs i
  in generate n generator

eulerForwardByFlowStep, eulerForwardAgainstFlowStep :: EulerStep
eulerForwardByFlowStep = eulerForwardStep gen where
  gen xs i = 
        re * ((xs ! i - 1) - 2 * (xs ! i) + (xs ! i + 1)) -
        st * ((xs ! i + 1) - xs ! i) + xs ! i

eulerForwardAgainstFlowStep = eulerForwardStep gen where
  gen xs i =
        re * ((xs ! i - 1) - 2 * (xs ! i) + (xs ! i + 1)) -
        st * ((xs ! i) - xs ! i - 1) + xs ! i
  
myplot name xs = plot' [] X11 $ Data3D [Color Red, Style Impulses, Title name] [] xs
main :: IO ()
main =
  do
    putStrLn $ show $ initial 200
    putStrLn $ (printf "st = %6.3f re = %6.3f" st re)
    writeFile "result.txt" $ join [printf "%f %f %f\n" x y z | (x, y, z) <- (ps)]
    return ()
  where
    result = eulerForwardAgainstFlow time_steps space_steps
    ys :: Vector Float
    gx :: Int -> Vector Float
    gx i = V.replicate space_steps ((fromIntegral i) * dt)
    n = space_steps - 1
    xxs = concat $ map (V.toList . gx) (nums 0 n)
    ys = V.iterateN space_steps (\x -> x + dx) x_left
    yys = concat $ replicate space_steps (V.toList ys)
    zzs = concat $ map (V.toList) result
    ps = zip3 xxs yys zzs

good a = and [notNaN a, notInf a]
change a = if good a then a else 0.0
change3 t@(a,b,c) = (change a, change b, change c)
notNaN = not . isNaN
notNaN3 (a, b, c) = and [(notNaN a), (notNaN b), (notNaN c)]

notInf = not . isInfinite
notInf3 (a, b, c) = and [(notInf a), (notInf b), (notInf c)]
good3 t = and [notNaN3 t, notInf3 t]

nums :: Int -> Int -> [Int]
nums = enumFromTo
