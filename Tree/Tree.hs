-- import Data.Tree
-- import Data.List (foldl')
treeScan :: (b -> a -> b) -> b -> Tree a -> Tree b
treeScan fn init (Node element children) 
    = Node (fn init element) 
        $ map (treeScan fn (fn init element)) children

treeMapAccum :: (b -> a -> (b,c)) -> b -> Tree a -> Tree c
treeMapAccum fn state (Node element children) 
    = Node elem $ map (treeMapAccum fn st) children where 
        (st,elem) = fn state element

treeZipWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
treeZipWith f (Node x xs) (Node y ys) 
    = Node (f x y) (zipWith (treeZipWith f) xs ys)

treePaths :: Tree a -> [[a]]
treePaths (Node element children) 
    | null children = [[element]] 
    | otherwise     = map (element :) . concat . map treePaths $ children

treeGetPath :: [Int] -> Tree a -> a
treeGetPath path tree 
    = rootLabel $ foldl' move tree path where
        move (Node _ children) pos = children !! pos

treePretty :: (Show a) => Tree a -> String
treePretty = drawTree . fmap show

treePrettyPrint :: (Show a) => Tree a -> IO ()
treePrettyPrint = putStrLn . treePretty

treeOfPaths :: Tree [Int]
treeOfPaths = go [] where 
    go p = Node p (map (\ i -> go (p++[i])) [0..])
