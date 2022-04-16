module AOC.Y2021.Heap where

data Heap a = E | T Int a (Heap a) (Heap a)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (T 1 x E E)

findMin :: Heap a -> Maybe a
findMin E = Nothing
findMin (T _ x _ _) = Just x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (T _ _ a b) = merge a b

merge :: Ord a => Heap a -> Heap a -> Heap a
merge E h = h
merge h E = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
  if x < y
    then makeT x a1 (merge b1 h2)
    else makeT y a2 (merge b2 h1)

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b =
  if rank a < rank b
    then T (rank b + 1) x b a
    else T (rank a + 1) x a b
  where
    rank E = 0
    rank (T n _ _ _) = n
