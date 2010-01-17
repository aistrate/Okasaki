module Ex05_07 where
       
import SplayHeap


sortWithHeap :: Ord a => [a] -> [a]
sortWithHeap = toList . fromList
