module PicoGUI.Abstract where

data MultiTree a = Node a [MultiTree a] deriving Show


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
        +>>
        empty 10

t3 = empty 0 +>> empty 1 +>> empty 2 +>> empty 3