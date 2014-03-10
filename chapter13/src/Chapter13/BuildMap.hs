module Chapter13.BuildMap where

import qualified Data.Map as Map

-- Functions to generate a Map that represents an association list as a map

al :: [(Integer, String)]
al = [(1,"one"), (2,"two"), (3,"three"), (4,"four")]

-- | Create a map representation of 'al' by converting the association
-- list using Map.fromList
mapFromAL :: Map.Map Integer String
mapFromAL = Map.fromList al

-- | Create a map representation of 'al' by doing a fold
mapFold :: Map.Map Integer String
mapFold = foldl (\map (k,v) -> Map.insert k v map) Map.empty al

-- | Manually create a map wit the elements of 'al' in it.
mapManual :: Map.Map Integer String
mapManual = Map.insert 2 "two" .
            Map.insert 4 "four" .
            Map.insert 1 "one" .
            Map.insert 3 "three" $ Map.empty
