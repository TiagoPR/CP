import List
import Probability
import Cp
import Data.Char


type Bit = Int
type Bin = [Bit ]
type Bit3 = (Bit, Bit, Bit)


dec2bin 0 = [ ]
dec2bin n = dec2bin m ++ [b ] where (m, b) = (div n  2, mod n 2)

tobin = rtrim 5 . pad 5 . dec2bin

bin2dec :: Bin -> Int
bin2dec [a ] = a
bin2dec b = bin2dec (init b) * 2 + last b

frombin = bin2dec . rtrim 5


rtrim n a = drop (length a - n) a

v3 (0, 0, 0) = 0
v3 (0, 0, 1) = 0
v3 (0, 1, 0) = 0
v3 (0, 1, 1) = 1
v3 (1, 0, 0) = 0
v3 (1, 0, 1) = 1
v3 (1, 1, 0) = 1
v3 (1, 1, 1) = 1

-- Triplicar info -> aplicar bflip em todos -> 


zeros = 0 : zeros

pad n x = take m zeros ++ x where m = n - length x

--bflips = propagate bflip

enc :: Char -> Bin
enc c = tobin (ord c - ord 'A')

dec :: Bin -> Char
dec b = chr (frombin b + ord 'A')

dec2 :: Dist Bin -> Dist Char
dec2 = fmap dec

bflip :: Bit -> Dist Bit
bflip 0 = D [(0, 0.96),(1, 0.04)]
bflip 1 = D [(1, 0.90),(0, 0.10)]

mmap f [] = return []
mmap f (h:t) = do {b<- f h ; x <- mmap f t; return(b:x)}

--propagate :: Monad m => (t -> m a) -> [t ] -> m [a ]
propagate f x = mmap bflip x
    --B É A DISTRIBUIÇAO JA FEITA

--bflip3 (a,b,c) = do {x<-bflip a; y<- bflip b; z<- bflip c; return(prod x(prod y z))}

--mmap2 f [] = return []
--mmap2 f (h:t) = do {c <- f (h,h,h) ;b <-fmap v3 c;x <- mmap2 f t; return(b:x)}

--propagate3 f x = mmap2 bflip3 x


transmit = dec2 . propagate bflip . enc

