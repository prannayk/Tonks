-- min heap structure implementation

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show,Eq,Ord)

sizeCalc :: Tree a -> Int
sizeCalc Empty = 0
sizeCalc (Node a tree1 tree2) = 1 + (sizeCalc tree1) + (sizeCalc tree2)

sizeMax :: Tree a -> Tree a -> (Tree a, Tree a)
sizeMax treei treej = if ((sizeCalc treej) > (sizeCalc treei)) then (treei,treej) else (treej,treei);

treeAdd :: Tree a -> a ->Tree a
treeAdd (Node thisElement treei treej) addElement
    | thisElement > addElement = Node addElement tree1 (treeAdd tree2 thisElement) 
    | otherwise = Node thisElement tree1 (treeAdd tree2 thisElement)
    where (tree1,tree2) = sizeMax treei treej
