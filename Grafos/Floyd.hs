
-- join para retornar algo despues de I/O
import Control.Monad (join, forM_)
import Data.List (union)
<<<<<<< HEAD
-- import Data.Pair 
import Data.Map as Map hiding (foldr, union, map, foldl)
=======
import Data.Map hiding (foldr, union, map)
>>>>>>> 7882368f42fea18f5b4dc6baedd164b83d70f62d
import Data.Maybe (fromJust, isJust)
--import Data.FiniteMap (insertList)
import Data.Semigroup
<<<<<<< HEAD
import Prelude hiding (lookup, filter, null)
import System.Environment (getArgs)
import Data.List.Split (chunksOf)
=======
import Prelude hiding (lookup, filter)
import System.Environment (getArgs)
>>>>>>> 7882368f42fea18f5b4dc6baedd164b83d70f62d

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
<<<<<<< HEAD
--showShortestPaths v g = mapM_ print $ toList $ findMinDistances v g
showShortestPaths v g = toList $ (findMinDistances v g)

--graph = Map.empty



insertList :: Ord key => [(key,elt)] -> Map key elt -> Map key elt
insertList xs m = foldl (\m (k, v) -> Map.insert k v m) m xs

main :: IO()
main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let items = (map read).words
   let str = map items (lines content) :: [[Int]]
   let nodesQty = str!!0!!0
   let edgesQty = str!!0!!1
   let myList = tail str 
   let edges = map  (\s -> ((s!!0, s!!1), s!!2)) myList
   let graph = insertList edges Map.empty

   let size = (length str) 
   return $! showShortestPaths [0..300] (Sum <$> graph)
   print size
   -- Ejecucion
   -- ghc Floyd.hs -o Floyd
   -- ./Floyd sparse1.txt

=======
showShortestPaths v g = mapM_ print $ toList $ findMinDistances v g
>>>>>>> 7882368f42fea18f5b4dc6baedd164b83d70f62d




main :: IO()
main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let str = map (read::Integer)(words content) -- :: [Integer]
   elemAt 1 (str)
   print str
   --let str = words content 
      --str = f str
      --data = executeList 0 list
   --showShortestPaths [1..4] (Sum <$> g)
   --let valor =  str!!1 
   --print str

-- Ejecucion Linux despues de instalar Haskell 
-- Creo que lo que falta es usar openFile o withFile para hacer el g (lista con Key:Value)
-- gchi 
-- :load Floyd
-- Para Weights as distances 
-- showShortestPaths [1..4] (Sum <$> g)