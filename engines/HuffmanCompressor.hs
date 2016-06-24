-- engine / module for compressing the input
-- initial implementation for strings, but will be followed by int64 abstractions for all kinds of data
-- lossless Huffman encoding compression algorithm has been implemented

module Compressor where

type Encoding = [(Char,[Int])]
type CodeSchema = [(Char,Int)]
data NodeTree = Node (Char,Int) NodeTree NodeTree | Empty deriving (Show)

instance Ord NodeTree where
    (Node (a,b) _ _) > (Node (c,d) _ _) = b > d
    (Node (a,b) _ _) < (Node (c,d) _ _) = b < d
    (Node (a,b) _ _) <= (Node (c,d) _ _) = b <= d
    (Node (a,b) _ _) >= (Node (c,d) _ _) = b >= d

instance Eq NodeTree where
    (Node (a,b) _ _) == (Node (c,d) _ _) = (b == d && a == c)
    (Node (a,b) _ _) /= (Node (c,d) _ _) = (b /= d || a /= c)

leftChild :: NodeTree -> NodeTree
leftChild (Node _ left _) = left

rightChild :: NodeTree -> NodeTree
rightChild (Node _ _ right) = right

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
quickSort (x:xs) = let leftList = [y | y <- xs, (second y) <= (second x)] in (quickSort leftList) ++ [x] ++ (quickSort [y | y<-xs, (second y) > (second x)])
quickSort [] = []
quickSortTree :: [NodeTree] -> [NodeTree]
quickSortTree (x:xs) = let leftList = [y | y <- xs, y <= x] in (quickSortTree leftList) ++ [x] ++ (quickSortTree [y | y <-xs, y > x])
quickSortTree [] = []
-- leafNode construction for further working on the tree
leafNodes :: CodeSchema -> [NodeTree]
leafNodes scheme = map (\x -> Node x Empty Empty) $ quickSort scheme

isLeaf :: NodeTree -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

valueRequest :: NodeTree -> Int
valueRequest (Node (_,b) _ _) = b

tagRequest ::  NodeTree -> Char
tagRequest (Node (a,_) _ _) = a

-- building the HUffman tree to calculate the Huffman Code
sumNew :: [NodeTree] -> NodeTree
sumNew list | (length list >= 2 )= Node ('/',(sum $ (map valueRequest list))) (minimum list) (maximum list)
sumNew (x:[]) = Node ('/',0) Empty Empty

recurBuild :: [NodeTree] -> NodeTree
recurBuild (x:[]) = x
recurBuild (x:xs) | xs /= [] = let sorted = quickSortTree (x:xs) in recurBuild $ (sumNew (take 2 sorted)):(drop 2 sorted)

calcEncoding :: NodeTree -> [Int] -> Encoding
calcEncoding tree vals
    | isLeaf tree = [(tagRequest tree,vals)]
    | otherwise = (calcEncoding (leftChild tree) (vals ++ [0])) ++ (calcEncoding (rightChild tree) (vals ++ [1]))


main :: IO ()
main = do
    let histo = histogram "kfgasjdhkajshdasjdk"
        leaves = leafNodes histo
        tree = recurBuild leaves
    putStrLn $ show (calcEncoding tree [])

