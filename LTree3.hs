module LTree3 where

import Cp
import Data.Monoid
import Control.Applicative
import List

type Tri = (Point, Side)

type Side = Int
type Point = (Int,Int)

-- (1) Datatype definition -----------------------------------------------------

data LTree3 a = Tri a | Nodo (LTree3 a) (LTree3 a) (LTree3 a) deriving (Eq, Show)

inLTree3 :: Tri a (LTree3 a, LTree3 a, LTree3 a) -> LTree3 a
inLTree3 = either Tri Nodo

outLTree3 :: LTree3 a -> Either a (LTree3 a,LTree3 a, LTree3 a)
outLTree3 (Tri a)       = i1 a
outLTree3 (Nodo (t1,t2,t3)) = i2 (t1,t2,t3)

baseLTree3 g f = g -|- f >< f >< f

-- (2) Ana + cata + hylo -------------------------------------------------------

recLTree3 f = baseLTree3 id f          -- that is:  id -|- (f >< f)

cataLTree3 g = g . (recLTree3 (cataLTree3 g)) . outLTree3

anaLTree3 f = inLTree3 . (recLTree3 (anaLTree3 f) ) . f

hyloLTree3 f g = cataLTree3 f . anaLTree3 g

-- (3) Map ---------------------------------------------------------------------

instance Functor LTree3
         where fmap f = cataLTree3 ( inLTree3 . baseLTree3 f id )

-- (4) Examples ----------------------------------------------------------------

sierpinski :: (Tri,Int) → [Tri]
sierpinski = folhasSierp · geraSierp

geraSierp :: (Tri,Int) → LTree3 Tri
geraSierp = anaLTree3 g2

folhasSierp :: LTree3 Tri → [Tri]
folhasSierp = cataLTree3 g1

g2 :: (Tri,Int) -> 
g2 (a,0) = a
g2 (((a,b),c),h+1) = t1 t2 t3 where
    l2 = c/2
    t1 = (l2,(a + l2, b))
    t2 = (l2,(a, b + l2))
    t3 = (l2,(a,b))

g1 :: LTree3 Tri -> [Tri]
g1 Tri a = [a]
g1 (LTree3 a) (LTree3 b) (LTree3 c) = concat