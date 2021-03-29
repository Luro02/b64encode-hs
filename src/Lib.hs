module Lib
    ( someFunc
    ) where
import           Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

toAdd :: Int -> Int -> Int
toAdd n m | n == m    = 0
          | otherwise = ((div n m + 1) * m) - n

binrpad :: Int -> [Int] -> [Int]
binrpad m list = list ++ replicate (toAdd (length list) m) 0

h2bin :: Int -> [Int] -> [Int]
h2bin number result
    | number == 0 = result
    | otherwise   = h2bin (div number 2) (result ++ [mod number 2])

dec2bin :: Int -> [Int]
dec2bin number = reverse (binrpad 8 (h2bin number []))

str2bin :: String -> [Int]
str2bin = concatMap (dec2bin . ord)

-- bin('M') = 0100_1101
--------------

-- TODO: not made by myself :(
group :: Int -> [a] -> [[a]]
group _ [] = []
group n list | n > 0     = take n list : group n (drop n list)
             | otherwise = error "Negative or zero n"

hb2d :: Int -> [Int] -> Int
hb2d result []   = result
hb2d result list = hb2d (2 * result + head list) (tail list)

bin2dec :: [Int] -> Int
bin2dec = hb2d 0

lu :: Int -> Char
lu i = (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['+', '/']) !! i

-- convert :: [[Int]] -> [Char] -> [Char]
-- convert []   result = result
-- convert list result = convert (tail list) result ++ [lu (head list)]

b64encode :: String -> String
b64encode input = do
    let binary = group 6 (str2bin input)
    let padded = init binary ++ [binrpad 6 (last binary)]
    map (lu . bin2dec) padded


main = do
    let example = "Some example string that should be encode"
    print (b64encode example)
