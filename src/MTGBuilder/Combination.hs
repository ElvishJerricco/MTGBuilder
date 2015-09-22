module MTGBuilder.Combination where

import Data.Set (Set)
import qualified Data.Set as Set

combinations :: Ord a => Int -> Set a -> Set (Set a)
combinations k xs = combinations' (Set.size xs) k xs
    where combinations' n k' s
            | k' == 0   = Set.singleton (Set.empty)
            | k' >= n   = Set.singleton (s)
            | null s    = Set.empty
            | otherwise = case Set.minView s of
                Just (y, ys)    -> Set.map (Set.insert y) (combinations' (n-1) (k'-1) ys) `Set.union` combinations' (n-1) k' ys
                Nothing         -> error "Invalid empty set"
