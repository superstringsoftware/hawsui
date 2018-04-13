module PicoGUI.Abstract where

-- import Data.Monoid hiding ((<>))

data MultiTree a = Node a [MultiTree a] deriving Show

instance Monoid a => Monoid (MultiTree a) where 
    (Node x xs) `mappend` (Node y ys) = Node (x `mappend` y) (xs ++ ys) 
    mempty = Node mempty []

instance Functor MultiTree where
    fmap f (Node x []) = Node (f x) []
    fmap f (Node x tr) = Node (f x) (map (fmap f) tr)

instance Foldable MultiTree where
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldMap f (Node x []) = f x
    foldMap f (Node x ts) = f x `mappend` mconcat (map (foldMap f) ts)

instance Traversable MultiTree where 
    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f (Node x []) = Node <$> f x <*> pure []
    traverse f (Node x ls) = Node <$> f x <*> traverse (traverse f) ls

{-
-- identity and homomorphism work, check other laws!!!
instance Applicative MultiTree where
    pure x = Node x []
    (Node f []) <*> tree = fmap f tree
    -- (Node f (g:gs)) <*> tree = ???
-}

empty :: a -> MultiTree a
empty el = Node el []


child :: a -> [MultiTree a]
child el = [empty el]

(<>) :: [MultiTree a] -> MultiTree a -> [MultiTree a]
(<>) lst x = lst ++ [x]

infixl 5 =>>
(=>>) :: MultiTree a -> [MultiTree a] -> MultiTree a
(=>>) (Node n cld) lst = Node n (cld ++ lst)


t1 = empty 0 =>> 
        [empty 1 =>>
            [empty 2] <> empty 3 <> empty 4]
        <>
        empty 10

(->>) :: MultiTree a -> MultiTree a -> MultiTree a
(->>) (Node n cld) el = Node n (cld ++ [el])

infixr 8 +>>
(+>>) :: MultiTree a -> MultiTree a -> MultiTree a
(+>>) (Node n cld) el = Node n (cld ++ [el])


t2 = empty 0 +>> 
        empty 1 ->> empty 2 ->> empty 3 ->> empty 4 
        ->>
        empty 10

t3 = empty 0 +>> empty 1 +>> empty 2 +>> empty 3