import LTree
import Cp

--both :: LTree Integer -> (Integer,Integer)
--both (Leaf a) = (a,a)
--both (Fork (a,b)) = (min (snd (both a)) (snd (both b)), max (fst (both a)) (fst (both b)))

-- Esta a dar errado:
--both (Leaf a) = (a,a)
--both (Fork (a,b)) = (uncurry min (both a),uncurry max (both b)) 

--both a = (bob a , alice a) 
both :: LTree Integer -> (Integer,Integer)
both = cataLTree g

g :: Either Integer ((Integer,Integer),(Integer,Integer)) -> (Integer,Integer)
g (Left a) = (a,a)
g (Right ((a,b),(c,d))) = (uncurry max(b,d),uncurry min(a,c))

bothb = split alice (bob)

alice :: LTree Integer -> Integer
alice (Leaf a) = a
alice (Fork (a,b)) = max (bob a) (bob b)

bob :: LTree Integer -> Integer
bob (Leaf a) = a
bob (Fork (a,b)) = min (alice a) (alice b)

t = Fork (
    Fork (
        Fork (Leaf 2, Leaf 7),
        Fork (Leaf 5, Leaf 4)),
    Fork (
        Fork (Leaf 8, Leaf 6),
        Fork (Leaf 1, Leaf 3))
        )