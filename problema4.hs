import List
import Probability

type Bit = Int
type Bin = [Bit ]
type Bit3 = (Bit, Bit, Bit)



enc :: Char -> Bin
enc c = tobin (ord c - ord 'A')

dec :: Bin -> Char
dec b = chr (frombin b + ord 'A')

dec' = fmap dec

bflip :: Bit -> Dist Bit
bflip 0 = D [(0, 0.96),(1, 0.04)]
bflip 1 = D [(1, 0.90),(0, 0.10)]

g = either nil g2
g2 (a, b) = insert (bflip a, b)

propagate :: Monad m => (t -> m a) -> [t ] -> m [a ]
propagate f = cataList (g f)

transmit = dec' · propagate bflip · enc

