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


zeros = 0 : zeros

pad n x = take m zeros ++ x where m = n - length x

--bflips = propagate bflip

enc :: Char -> Bin
enc c = tobin (ord c - ord 'A')

dec :: Bin -> Char
dec b = chr (frombin b + ord 'A')

--dec' = fmap dec

bflip :: Bit -> Dist Bit
bflip 0 = D [(0, 0.96),(1, 0.04)]
bflip 1 = D [(1, 0.90),(0, 0.10)]



propagate :: Monad m => (t -> m a) -> [t ] -> m [a ]

propagate f = mcataList 

--transmit = dec' . propagate bflip . enc

