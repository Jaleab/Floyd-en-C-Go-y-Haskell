-- join para retornar algo despues de I/O
import Control.Monad (join)
import Data.List (union)
import Data.Map hiding (foldr, union)
import Data.Maybe (fromJust, isJust)
import Data.Semigroup
import Prelude hiding (lookup, filter)

data Shortest b a = Shortest { distance :: a, path :: [b] }
                  deriving Show

instance (Ord a, Eq b) => Semigroup (Shortest b a) where
  a <> b = case distance a `compare` distance b of
    GT -> b
    LT -> a
    EQ -> a { path = path a `union` path b }

floydWarshall v dist = foldr innerCycle (Just <$> dist) v
  where
    innerCycle k dist = (newDist <$> v <*> v) `setTo` dist
      where
        newDist i j =
          ((i,j), do a <- join $ lookup (i, k) dist
                     b <- join $ lookup (k, j) dist
                     return $ Shortest (distance a <> distance b) (path a))
 
        setTo = unionWith (<>) . fromList

buildPaths d = mapWithKey (\pair s -> s { path = buildPath pair}) d
  where
    buildPath (i,j)
      | i == j    = [[j]]
      | otherwise = do k <- path $ fromJust $ lookup (i,j) d
                       p <- buildPath (k,j)
                       [i : p]

findMinDistances v g =
  let weights = mapWithKey (\(_,j) w -> Shortest w [j]) g
      trivial = fromList [ ((i,i), Shortest mempty []) | i <- v ]
      clean d = fromJust <$> filter isJust (d \\ trivial)
  in buildPaths $ clean $ floydWarshall v (weights <> trivial)

-- Return container with all of the elements from the list (Key:Value)
g = fromList [((2,1), 4)
             ,((2,3), 3)
             ,((1,3), -2)
             ,((3,4), 2)
             ,((4,2), -1)]
showShortestPaths v g = mapM_ print $ toList $ findMinDistances v g

-- Ejecucion Linux despues de instalar Haskell 
-- Creo que lo que falta es usar openFile o withFile para hacer el g (lista con Key:Value)
-- gchi 
-- :load Floyd
-- Para Weights as distances 
-- showShortestPaths [1..4] (Sum <$> g) 