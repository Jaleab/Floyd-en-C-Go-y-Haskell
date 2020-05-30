import Control.Monad (join, forM_)
import Data.List (union)
import Data.Map as Map hiding (foldr, union, map, foldl)
import Data.Maybe (fromJust, isJust)
import Data.Semigroup
import Prelude hiding (lookup, filter, null)
import System.Environment (getArgs)

--import Data.Time
--import System.Posix.Unistd

import Text.Printf
import Control.Exception
import System.CPUTime

data Shortest b a = Shortest { distance :: a, path :: [b] }
                  deriving Show

instance (Ord a, Eq b) => Semigroup (Shortest b a) where
  -- <> mappend: sirve para unir
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
             ,((4,2), -1)
             , ((5,5), 0)]
--showShortestPaths v g = mapM_ print $ toList $ findMinDistances v g
showShortestPaths v g = (findMinDistances v g)


insertList :: Ord key => [(key,elt)] -> Map key elt -> Map key elt
insertList xs m = foldl (\m (k, v) -> Map.insert k v m) m xs

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    --printf "Computation time: %0.3f sec\n" (diff :: Double)
    printf "%f" (diff :: Double)
    return v


main :: IO()
main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let items = (map read).words
   let str = map items (lines content) :: [[Int]]
   let nodesQty = str!!0!!0
   --let edgesQty = str!!0!!1
   let myList = tail str 
   let edges = map  (\s -> ((s!!0, s!!1), s!!2)) myList
   let graph = insertList edges Map.empty

   time $ (showShortestPaths [0..nodesQty] (Sum <$> graph)) `seq` return ()

   --t1 <- getCurrentTime
   --usleep 100000 -- 100ms
   --return $! showShortestPaths [0..nodesQty] (Sum <$> graph)
   -- -- getCurrentTime/UTCTime has a precision of 1 picosecond, full precision is used by default
   --t2 <- getCurrentTime
   --putStr . show $ (diffUTCTime t2 t1)
   --print (diffUTCTime t2 t1)   
   
   -- Ejecucion
   -- ghc Floyd.hs -o Floyd
   -- ./Floyd sparse1.txt


-- Ejecucion Linux despues de instalar Haskell 
-- Creo que lo que falta es usar openFile o withFile para hacer el g (lista con Key:Value)
-- gchi 
-- :load Floyd
-- Para Weights as distances 
-- showShortestPaths [1..4] (Sum <$> g)
