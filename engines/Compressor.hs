-- engine / module for compressing the input
-- initial implementation for strings, but will be followed by int64 abstractions for all kinds of data
-- lossless Huffman encoding compression algorithm has been implemented

allText :: String
allText = ['a'..'z']++['A'..'Z']++(concat $ map show [1..9])

countN :: String -> Char -> [(Char,Int)]
countN string a = [(a,length $ filter (==a) string)]

histogram :: String -> [(Char, Int)]
histogram string = filter (\(_,n) -> n/=0) $ allText >>= countN string

-- huffman encoding takes input text and it's histogram
