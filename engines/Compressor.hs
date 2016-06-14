-- engine / module for compressing the input
-- initial implementation for strings, but will be followed by int64 abstractions for all kinds of data
-- lossless Huffman encoding compression algorithm has been implemented

module Compressor where

type CodeSchema = [(Char,Int)]
data NodeTree = Node (Char,Int) NodeTree NodeTree | Empty deriving (Show)

instance Ord NodeTree where
    (Node (a,b) _ _) > (Node (c,d) _ _) = b > d
    (Node (a,b) _ _) < (Node (c,d) _ _) = b < d
    (Node (a,b) _ _) <= (Node (c,d) _ _) = b <= d
    (Node (a,b) _ _) >= (Node (c,d) _ _) = b >= d

instance Eq NodeTree where
    (Node (a,b) _ _) == (Node (c,d) _ _) = b == d
    (Node (a,b) _ _) /= (Node (c,d) _ _) = b /= d    

second :: (a,b) -> b
second (_,b) = b

allText :: String
allText = ['a'..'z']++['A'..'Z']++(concat $ map show [1..9])

countN :: String -> Char -> CodeSchema
countN string a = [(a,length $ filter (==a) string)]

histogram :: String -> CodeSchema
histogram string = filter (\(_,n) -> n/=0) $ allText >>= countN string

-- huffman encoding takes input text and it's histogram
--sort functions for codeScheme
quickSort :: CodeSchema -> CodeSchema
quickSort (x:xs) = let leftList = [y | y <- xs, (second y) < (second x)] in (quickSort leftList) ++ [x] ++ (quickSort (filter (\x -> not (x `elem` (leftList ++ [x]))) (x:xs)))
quickSortTree :: [NodeTree] -> [NodeTree]
quickSortTree (x:xs) = let leftList = [y | y <- xs, y < x] in (quickSortTree leftList) ++ [x] ++ (quickSortTree (filter (\x -> not (x `elem` (leftList ++ [x]))) (x:xs)))

-- leafNode construction for further working on the tree
leafNodes :: CodeSchema -> [NodeTree]
leafNodes scheme = map (\x -> Node x Empty Empty) $ quickSort scheme

isLeaf :: NodeTree -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

-- building the HUffman tree to calculate the Huffman Code
sumNew :: [NodeTree] -> NodeTree


recurBuild :: [NodeTree] -> NodeTree
recurBuild (x:[]) = x
recurBuild (x:xs) | xs /= [] = let sorted = quickSortTree (x:xs) in recurBuild $ (sumNew(take 2 sorted)):(drop 2 (x:xs))

