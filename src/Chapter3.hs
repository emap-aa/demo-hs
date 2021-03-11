
module Chapter3 where

div1 :: Integral a => a -> a -> a
div1 x y = floor (fromIntegral x / fromIntegral y)

floor1 :: Float -> Integer
floor1 = read . takeWhile (/= '.') . show
