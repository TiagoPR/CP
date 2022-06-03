import LTree


--both :: LTree Integer -> (Integer,Integer)
--both (Leaf a) = (a,a)
--both (Fork (a,b)) = (min (snd (both a)) (snd (both b)), max (fst (both a)) (fst (both b)))

-- Esta a dar errado:
--both (Leaf a) = (a,a)
--both (Fork (a,b)) = (uncurry min (both a),uncurry max (both b)) 

--both a = (bob a , alice a) 

g = either (split id id) (split (uncurry min ((snd fst) >< (snd snd)) ) (uncurry max ((fst snd) >< (fst fst)))) 

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