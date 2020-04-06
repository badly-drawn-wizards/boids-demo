{-# LANGUAGE TupleSections, DataKinds, TypeApplications, DeriveGeneric, BangPatterns, BlockArguments, FlexibleInstances #-}

module Main where

import Control.Lens

import Control.Monad
import Control.Monad.Random.Class
import Data.Generics.Product
import Data.Semigroup
import Data.Monoid
import Data.Trees.KdTree as K
import Data.Function
import GHC.Generics hiding (to)
import GHC.Float (float2Double)
import Linear.V2
import Linear.Vector
import Linear.Metric
import Graphics.Gloss


data Boid = Boid
  { pos :: !(V2 Float)
  , vel :: !(V2 Float)
  , acc :: !(V2 Float)

  , flockNum :: !Int
  , airResistance :: !Float
  , yearning :: !Float
  , infected :: Bool

  } deriving (Eq, Generic)

instance K.Point (V2 Float) where
  dimension _ = 2
  coord 0 = float2Double . view _x
  coord 1 = float2Double . view _y
  coord _ = error "Invalid coordinate"
  dist2 x y = float2Double $ qd x y

instance K.Point Boid where
  dimension = dimension . pos
  coord d = coord d . pos
  dist2 = dist2 `on` pos

data Scene = Scene
  { sceneBoids :: KdTree Boid
  } deriving Generic

type TimeDelta = Float


_norm :: (Floating a, Eq a) => Traversal' (V2 a) a
_norm = filtered (/= zero) . lens norm (\ v d -> signorm v ^* d)

updateBoid :: TimeDelta -> KdTree Boid -> (Boid -> Boid)
updateBoid dt boids boid = boid
      & the @"pos" +~ dt *^ vel boid
      & the @"vel" +~ dt *^ acc boid
      & the @"acc" .~ sumV
        [ center & maybe zero (\ center' -> (center' ^-^ pos boid) & _norm %~ cohesion)
        , heading & maybe zero (\ heading' -> (heading' ^-^ vel boid ) & _norm %~ alignment)
        , vel boid & _norm %~ \ speed -> (targetSpeed - speed) * targetSpeedWeight
        , -pos boid ^* yearning boid
        ]
      & the @"infected" %~ maybe id ((||) . (< infectionDistance)) closestInfected
  where

    targetSpeed = 50
    targetSpeedWeight = 10
    alignment = const 20
    cohesion d = (1 - (50 / d)) * 50
    infectionDistance = 30


    neighbours :: Fold (KdTree Boid) Boid
    neighbours = to (\kd -> drop 1 $ kNearestNeighbors kd (flockNum boid + 1) boid) . folded

    (center, heading, closestInfected) = boids
      & foldMapOf neighbours
      (\ b ->
          ( Sum . pos $ b
          , Sum . vel $ b
          , guard (infected b) >> (Just . Min) (pos boid `distance` pos b)
          , Sum 1 :: Sum Int))
      & (\ (Sum totalPos, Sum totalHeading, closestInfected,  Sum count) ->
           ( guard (count > 0) >> pure (totalPos ^/ fromIntegral count)
           , guard (count > 0) >> pure (totalHeading ^/ fromIntegral count)
           , getMin <$> closestInfected))

updateBoids :: TimeDelta -> KdTree Boid -> KdTree Boid
updateBoids dt boids = K.fromList (boids ^.. folded . to (updateBoid dt boids))

updateScene :: TimeDelta -> Scene -> Scene
updateScene dt = the @"sceneBoids" %~ updateBoids dt


displayScene :: Scene -> Picture
displayScene = foldOf $ the @"sceneBoids"
  . folded
  . to displayBoid
  where
    displayBoid boid =
      let
        (V2 x y) = pos boid
      in
        translate x y $ color (if infected boid then red else blue) $ circleSolid $ 5

generateScene :: MonadRandom m => m Scene
generateScene = replicate 200 generateBoid & sequenceOf traversed & fmap K.fromList & fmap Scene

generateBoid :: MonadRandom m => m Boid
generateBoid = do
    posR <- (V2 <$> getRandomR (-1, 1) <*> getRandomR (-1, 1)) ^* 500
    velR <- (V2 <$> getRandomR (-1, 1) <*> getRandomR (-1, 1)) ^* 50
    colorR <- makeColor <$> getRandomR (0.3, 1) <*> getRandomR (0.3, 1) <*> getRandomR (0.3, 1) <*> pure 1
    return $ Boid
      { pos = posR
      , vel = velR
      , acc = zero
      , infected = False
      , flockNum = 5
      , yearning = 0.001
      , airResistance = 0.1
      }


main :: IO ()
main = do
  initialScene <- generateScene
  simulate FullScreen black 20 initialScene displayScene (const updateScene)

-- try do some profiling
-- maybe have a space
