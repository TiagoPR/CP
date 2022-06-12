module LTree3 where

import Cp
import Data.Monoid
import Control.Applicative
import List
import Svg


type Tri = (Point, Side)

type Side = Integer
type Point = (Integer,Integer)
-- (1) Datatype definition -----------------------------------------------------

data LTree3 a = Val a | Nodo (LTree3 a, (LTree3 a, LTree3 a)) deriving (Eq, Show)

inLTree3 :: Either a (LTree3 a, (LTree3 a, LTree3 a)) -> LTree3 a
inLTree3 = either Val Nodo

outLTree3 :: LTree3 a -> Either a (LTree3 a,(LTree3 a, LTree3 a))
outLTree3 (Val a)       = i1 a
outLTree3 (Nodo (t1,(t2,t3))) = i2 (t1,(t2,t3))

baseLTree3 g f = g -|- f >< (f >< f)

-- (2) Ana + cata + hylo -------------------------------------------------------

recLTree3 f = baseLTree3 id f          -- that is:  id -|- (f >< f)

cataLTree3 g = g . (recLTree3 (cataLTree3 g)) . outLTree3

anaLTree3 f = inLTree3 . (recLTree3 (anaLTree3 f) ) . f

hyloLTree3 f g = cataLTree3 f . anaLTree3 g

-- (3) Map ---------------------------------------------------------------------

instance Functor LTree3
         where fmap f = cataLTree3 ( inLTree3 . baseLTree3 f id )

-- (4) Examples ----------------------------------------------------------------



sierpinski :: (Tri,Int) -> [Tri]
sierpinski = folhasSierp . geraSierp

geraSierp :: (Tri,Int) -> LTree3 Tri
geraSierp = anaLTree3 g2

folhasSierp :: LTree3 Tri -> [Tri]
folhasSierp = cataLTree3 g1

g2 :: (Tri,Int) -> Either Tri ((Tri,Int),((Tri,Int),(Tri,Int)))
g2 (a,0) = i1 a
g2 (((a,b),c),h) = i2 ((t1,h-1), ((t2,h-1) ,(t3,h-1))) where
    l2 = div c 2
    t1 = ((a + l2, b),l2)
    t2 = ((a, b + l2),l2)
    t3 = ((a,b),l2)

g1 :: Either Tri ([Tri],([Tri],[Tri])) -> [Tri]
g1 (Left a) = [a]
g1 (Right (a,(b,c))) = a ++ (b ++ c)

type Svg = String

tri2svg :: Tri -> Svg
tri2svg (p, c) = (red . polyg) [p, p .+ (0, c), p .+ (c, 0)]


base = ((0, 0), 32)

desenha x = picd' [scale 0.44 (0, 0) (x >>= tri2svg)]

teste = desenha (sierpinski (base, 3))
