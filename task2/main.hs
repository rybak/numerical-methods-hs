{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, MultiParamTypeClasses, RankNTypes, UnboxedTuples, FunctionalDependencies, RecordWildCards, ExistentialQuantification, BangPatterns #-}


import Prelude ( Show(..)
               , error, undefined
               , Bool(..), (&&), (||), not
               , Int, div
               , String
               , Float, Double
               , fromIntegral, realToFrac
               , pi, sin, cos, asin, acos, atan, atan2, sqrt
               , Monad(..), (.), ($),
               )
import qualified Prelude as P
 
-- Rybak imports
import Data.List
import Text.Printf
import System.Environment(getArgs)
import Data.Sequence (index, iterateN)
import Graphics.EasyPlot
-- Algebra
 
class Monoid a where
  mzero   :: a
  mappend :: a -> a -> a
  mconcat :: [ a ] -> a
  mconcat = P.foldr mappend mzero
 
-- Instances
 
instance Monoid Float where
  mzero = 0
  mappend = (P.+)
 
instance Monoid Double where
  mzero = 0
  mappend = (P.+)
 
--
 
class Monoid a => Group a where
  ginv :: a -> a
  gsub :: a -> a -> a
  gsub x y = mappend x (ginv y)
 
instance Group Float where
  ginv = P.negate
  gsub = (P.-)
 
instance Group Double where
  ginv = P.negate
  gsub = (P.-)
 
---
 
class Group a => Ring a where -- very Monoidy
  rone  :: a
  rmult :: a -> a -> a
 
instance Ring Float where
  rone = 1
  rmult = (P.*)
 
instance Ring Double where
  rone = 1
  rmult = (P.*)
 
--
 
class Ring a => Field a where
  recip  :: a -> a
 
  (/)    :: a -> a -> a
  (/) x y = rmult x (recip y)
 
(+) :: Monoid a => a -> a -> a
(+) = mappend
 
negate :: Group a => a -> a
negate = ginv
 
(-) :: Group a => a -> a -> a
(-) = gsub
 
(*) :: Ring a => a -> a -> a
(*) = rmult
 
instance Field Float where
  recip = P.recip
  (/) = (P./)
 
instance Field Double where
  recip = P.recip
  (/) = (P./)
 
-- Vector Spaces
 
class (Group v, Field s) => VectorSpace v s | v -> s where
  (^*) :: s -> v -> v
  (^/) :: v -> s -> v
  (^/) v x = (^*) (recip x) v
 
--
 
class BasicVector v where
  diagonal :: s -> v s
  vmap     :: (s -> s) -> v s -> v s
  vzip     :: (s -> s -> s) -> v s -> v s -> v s
  vfold    :: (s -> s -> s) -> v s -> s
 
instance (Monoid s, BasicVector v) => Monoid (v s) where
  mzero = diagonal mzero
  mappend = vzip mappend
 
instance (Group s, BasicVector v) => Group (v s) where
  ginv = vmap ginv
  gsub = vzip gsub
 
instance (Field s, BasicVector v) => VectorSpace (v s) s where
  (^*) s = vmap (s *)
  (^/) v s = vmap (/ s) v
 
-- 2D Vectors
 
data Vector2 a = Vector2 !a !a
                deriving Show
 
instance BasicVector Vector2 where
  diagonal s = Vector2 s s
  vmap f (Vector2 x y) = Vector2 (f x) (f y)
  vzip f (Vector2 x y) (Vector2 x' y') = Vector2 (f x x') (f y y')
  vfold o (Vector2 x y) = x `o` y
 
-- 3D Vectors
 
data Vector3 a = Vector3 !a !a !a
                deriving Show
 
instance BasicVector Vector3 where
  diagonal s = Vector3 s s s
  vmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)
  vzip f (Vector3 x y z) (Vector3 x' y' z') = Vector3 (f x x') (f y y') (f z z')
  vfold o (Vector3 x y z) = x `o` y `o` z
 
--
 
data Color4 a = Color4 !a !a !a !a
                deriving Show
 
instance BasicVector Color4 where
  diagonal s = Color4 s s s s
  vmap f (Color4 x y z a) = Color4 (f x) (f y) (f z) (f a)
  vzip f (Color4 x y z a) (Color4 x' y' z' a') = Color4 (f x x') (f y y') (f z z') (f a a')
  vfold o (Color4 x y z a) = x `o` y `o` z `o` a
 
-- instance Functor [] where
--   fmap = P.map
--  
-- instance Functor ((->) c) where
--   fmap f g c = f (g c)
 
--class Applicative f where
--  pure :: a -> f a
--  (<$>) :: f (a -> b) -> f a -> f b
--instance Functor ((←) c) where
--  fmap f g c = hole
 
--
 
infixr 4 +
infixr 4 -
infixr 5 ^*
infixr 5 ^/
infixr 5 *
infixr 5 /
 
dt, dx, u, kappa :: Float
dt = 0.00001
dx = 0.1
u = 5000.0
kappa = 300.0
st = u * dt / dx
re = kappa * dt / dx / dx

time_steps, space_steps :: Int
time_steps = 200
space_steps = 200

initial :: Int -> [Float]
initial n = P.replicate (n `div` 2) 1.0 ++ P.replicate (n P.- n `div` 2) 0.0

type MethodType = Float -> Float -> Float -> Float -> Int -> Int -> [Float]

eulerFA :: MethodType
eulerFA dt dx u k tn sn = undefined

main :: P.IO ()
main = do
  P.putStrLn "task2"
  P.putStrLn $ show $ initial 10
