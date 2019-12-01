module NonEmptyZipper where

data Zipper a = Zipper {
    left :: [a],
    curr :: a,
    right :: [a]
} deriving Show

instance Functor Zipper where
    fmap f (Zipper l c r) = Zipper (fmap f l) (f c) (fmap f r)

goRight :: Zipper a -> Zipper a
goRight (Zipper l a (x:xs)) = Zipper (a:l) x xs
goRight (Zipper l a []) = let (x:xs) = reverse (a:l) in Zipper [] x xs

goLeft :: Zipper a -> Zipper a
goLeft (Zipper (x:xs) a r) = Zipper xs x (a:r)
goLeft (Zipper [] a r) = let (x:xs) = reverse (a:r) in Zipper xs x []

-- deletes the current element, then moves to the right
deleteCurrent :: Zipper a -> Zipper a
deleteCurrent (Zipper [] _ []) = error "cannot delete from one element zipper"
deleteCurrent (Zipper l _ (x:r)) = Zipper l x r
deleteCurrent (Zipper (x:l) _ []) = goRight $ Zipper l x []

insertAfterCurrent :: Zipper a -> a -> Zipper a
insertAfterCurrent (Zipper l x r) new = Zipper (x:l) new r

updateCurrent :: (a -> a) -> Zipper a -> Zipper a
updateCurrent f (Zipper l x r) = Zipper l (f x) r

getCurrent :: Zipper a -> a
getCurrent (Zipper _ x _) = x

toList :: Zipper a -> [a]
toList (Zipper l c r) = reverse l ++ (c:r)

fromList :: [a] -> Zipper a
fromList [] = error "empty list cannot be made to zipper"
fromList (x:xs) = Zipper [] x xs

offset :: Zipper a -> Int
offset = length . left
