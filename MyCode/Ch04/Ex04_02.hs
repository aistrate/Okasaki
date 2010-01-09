module Ex04_02 where


insertionSort :: Ord a => [a] -> [a]

-- insertionSort xs = reverse $ insertionSort' [] xs
--   where insertionSort' ys []     = ys
--         insertionSort' ys (x:xs) = insertionSort' (insert x ys) xs
--         insert x [] = [x]
--         insert x ys'@(y:ys) | x >= y    = x : ys'
--                             | otherwise = y : insert x ys

-- insertionSort = reverse . foldl insert []
--   where insert [] x = [x]
--         insert ys'@(y:ys) x | x >= y    = x : ys'
--                             | otherwise = y : insert ys x

-- insertionSort = reverse . foldl insert []
--   where insert ys x = let (gt, le) = span (> x) ys
--                       in gt ++ (x : le)

insertionSort = foldl insert []
  where insert [] x = [x]
        insert ys'@(y:ys) x | x < y     = x : ys'
                            | otherwise = y : insert ys x
