
module Salt.Data.List where
import Data.Set as Set


-- | Get the list of duplicate values in a list.
duplicates :: Ord a => [a] -> [a]
duplicates xx
 = go Set.empty Set.empty xx
 where
        go _   dups []  = Set.toList dups
        go acc dups (x : xs)
         | Set.member x acc     = go acc (Set.insert x dups) xs
         | otherwise            = go (Set.insert x acc) dups xs

