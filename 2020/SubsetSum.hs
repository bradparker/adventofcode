{-# OPTIONS_GHC -Wall #-}

module SubsetSum
  ( sumToTarget,
  )
where

import Control.Monad ((<=<))
import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (mapMaybe)

lessThanOrEqual :: Int -> IntMap a -> IntMap a
lessThanOrEqual n im =
  case IntMap.splitLookup n im of
    (less, mv, _) -> maybe less (\v -> IntMap.insert n v less) mv

updateSums :: Int -> IntMap (IntMap [Int]) -> Int -> IntMap (IntMap [Int])
updateSums target sums n =
  sums
    & lessThanOrEqual (target - n)
    & IntMap.foldrWithKey
      ( \s ->
          IntMap.insertWith (<>) (s + n)
            . IntMap.foldMapWithKey
              (\l -> IntMap.singleton (l + 1) . (n :))
      )
      sums
    & IntMap.insertWith (<>) n (IntMap.singleton 1 [n])

sumToTarget :: Int -> Int -> [Int] -> [[Int]]
sumToTarget size target =
  mapMaybe (IntMap.lookup size <=< IntMap.lookup target)
    . scanl (updateSums target) IntMap.empty
