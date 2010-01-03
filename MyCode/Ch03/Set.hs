{-# LANGUAGE MultiParamTypeClasses #-}

module Set where


class Set s a where
  emptySet :: s a
  insertSet :: a -> s a -> s a
  memberSet :: a -> s a -> Bool


fromList :: Set s a => [a] -> s a
fromList = foldl (flip insertSet) emptySet
