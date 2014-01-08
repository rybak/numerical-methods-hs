{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, MultiParamTypeClasses, RankNTypes, UnboxedTuples, FunctionalDependencies, RecordWildCards, ExistentialQuantification, BangPatterns #-}

--import Prelude hiding (replicate, (++))
import Data.List hiding (replicate, (++))
import Data.Vector (Vector (..), (!), generate)
import qualified Data.Vector as V

import Text.Printf
import Control.Monad

-- gnuplot
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

-- /gnuplot
 
import System.Environment
import System.Console.GetOpt

type FPType = Double

--re = kappa * dt / dx / dx
x_left, x_right :: FPType
x_left = 1.0
x_right = 0.0

time_steps, space_steps :: Int
time_steps = 200
space_steps = 200

initial :: Int -> Vector FPType
initial size = V.replicate (size `div` 2) 1.0 V.++ V.replicate (size - (size `div` 2)) 0.0

data Params = Container {kappa :: FPType, u :: FPType, dx :: FPType, dt :: FPType, st :: FPType, re :: FPType} deriving Show
                            
type MethodType = Params -> Int -> Int -> [Vector FPType]
type EulerStep = Params -> Vector FPType -> Vector FPType

eulerForward :: EulerStep -> MethodType
eulerForward step params tn sn = take tn (iterate (step params) (initial sn))

eulerForwardAgainstFlow, eulerForwardByFlow :: MethodType
eulerForwardByFlow = eulerForward eulerForwardByFlowStep -- #1
eulerForwardAgainstFlow = eulerForward eulerForwardAgainstFlowStep -- #2

type GenType = (Params -> Vector FPType -> Int -> FPType)
eulerForwardStep :: GenType -> EulerStep
eulerForwardStep gen params xs = let
  n = V.length xs
  generator i
    | (i == 0)     = x_left
    | (i == n - 1) = x_right
    | otherwise    = gen params xs i
  in generate n generator

eulerForwardByFlowStep, eulerForwardAgainstFlowStep :: EulerStep
eulerForwardByFlowStep = eulerForwardStep gen where
  gen :: GenType
  gen p xs i = 
        (re p) * ((xs ! i - 1) - 2 * (xs ! i) + (xs ! i + 1)) -
        (st p) * ((xs ! i + 1) - xs ! i) + xs ! i

eulerForwardAgainstFlowStep = eulerForwardStep gen where
  gen p xs i =
        (re p) * ((xs ! (i - 1)) - 2 * (xs ! i) + (xs ! (i + 1))) -
        (st p) * ((xs ! i) - (xs ! (i - 1))) + (xs ! i)
  
defltOpts :: Graph.C graph => Opts.T graph
defltOpts = Opts.grid True $ Opts.key False $ Opts.deflt

myplot :: String -> [[(FPType, FPType, FPType)]] -> Frame.T (Graph3D.T FPType FPType FPType)
myplot methodTitle res = Frame.cons (
    Opts.title methodTitle $
    Opts.xLabel "time" $
    Opts.yLabel "space" $
    defltOpts
    )
    $ Plot3D.mesh res

readFPType :: String -> FPType
readFPType = read

options :: [OptDescr (Options -> Options)]
options = [
  Option ['k'] ["kappa"]
  (ReqArg (\ d opts -> opts { optKappa = readFPType d }) "NUM")
  "kappa parameter",
  Option ['u'] []
  (ReqArg (\ d opts -> opts { optU = readFPType d }) "NUM")  "u parameter",
  Option ['x'] ["delta-x"]
  (ReqArg (\ d opts -> opts { optDX = readFPType d }) "NUM") "space step",
  Option ['t'] ["delta-t"]
  (ReqArg (\ d opts -> opts { optDT = readFPType d }) "NUM") "time step"
  ]
 
data Options = Options
     { optKappa :: FPType
     , optU     :: FPType
     , optDX    :: FPType 
     , optDT    :: FPType 
     } deriving Show
defaultDT, defaultDX, defaultU, defaultKappa :: FPType
defaultDT = 0.000001
defaultDX = 0.1
defaultU = 5000.0
defaultKappa = 300.0

calcSt, calcRe :: FPType -> FPType -> FPType -> FPType
calcSt u dx dt = (u * dt) / dx
calcRe kappa dx dt = (kappa * dt) / (dx * dx)
defaultSt, defaultRe :: FPType
defaultSt = calcSt defaultU defaultDX defaultDT
defaultRe = calcRe defaultKappa defaultDX defaultDT

defaultOptions :: Options
defaultOptions = Options
     { optKappa = defaultKappa
     , optU = defaultU
     , optDX = defaultDX
     , optDT = defaultDT
     }
myParams :: [String] -> IO (Params, [String])
myParams argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (fromOptions (foldl (flip id) defaultOptions o), n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: main [OPTION...]"

calcStFromParams, calcReFromParams :: Params -> FPType
calcStFromParams p = calcSt (u p) (dx p) (dt p)
calcReFromParams p = calcRe (kappa p) (dx p) (dt p)
recalcParams :: Params -> Params
recalcParams p = p { st = calcStFromParams p, re = calcReFromParams p}

fromOptions :: Options -> Params
fromOptions o = recalcParams $ Container {kappa = optKappa o, u = optU o, dx = optDX o, dt = optDT o, st = 0.0, re = 0.0}

main :: IO ()
main = do
    argv <- getArgs
    (o, ss) <- myParams argv
    putStrLn $ show o
    putStrLn $ show ss
    _ <- GP.plotDefault(myplot "Euler explicit against flow" $ (myGroup time_steps (ps eulerForwardAgainstFlow o)))
    putStrLn $ (printf "st = %6.3f re = %6.3f" (st o) (re o))
    writeFile "result.txt" $ join [printf "%f %f %f\n" x y z | (x, y, z) <- (ps eulerForwardAgainstFlow o)]
    return ()
  where
    result m p = m p time_steps space_steps
    ys :: Params -> Vector FPType
    genTimes :: Params -> Int -> Vector FPType
    genTimes o i = V.replicate space_steps ((fromIntegral i) * (dt o))
    n = time_steps - 1
    xxs p = concat $ map (V.toList . (genTimes p) ) (nums 0 n)
    ys p = V.iterateN space_steps (\x -> x + (dx p)) 0.0
    yys p = concat $ replicate space_steps (V.toList (ys p))
    ps m p = zip3 (xxs p) (yys p) (concat $ map (V.toList) (result m p))

myGroup :: Int -> [a] -> [[a]]
myGroup _ [] = []
myGroup n xs = (take n xs) : (myGroup n (drop n xs))

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
