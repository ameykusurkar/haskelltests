type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

key :: BinTree a -> a
key (Node v _ _) = v

rank :: BinTree a -> Int
rank (Node _ r _) = r

children :: BinTree a -> [BinTree a]
children (Node _ _ ts) = ts

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
-- pre: Both trees must have the same rank
combineTrees t1 t2
  | key t1 < key t2 = Node (key t1) (rank t1 + 1) (t2:(children t1))
  | otherwise       = Node (key t2) (rank t2 + 1) (t1:(children t2))

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
-- pre: The heap is non-empty
extractMin = minimum . (map key)


mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h [] = h
mergeHeaps [] h = h
mergeHeaps h@(t:ts) h'@(t':ts')
  | rank t < rank t'  = t : (mergeHeaps ts  h')
  | rank t > rank t'  = t': (mergeHeaps ts' h )
  | rank t == rank t' = mergeHeaps [combineTrees t t'] (mergeHeaps ts ts')

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert x h = mergeHeaps [Node x 0 []] h

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin (t:ts)
  | key t == minVal = mergeHeaps childHeap ts
  | otherwise       = mergeHeaps [t] (deleteMin ts)
    where 
      minVal    = extractMin (t:ts)
      childHeap = (reverse . children) t

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove x 
  = undefined 

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin
  = undefined

binSort :: Ord a => [a] -> [a]
binSort = extractHeap . makeHeap
  where
    makeHeap xs    = foldr insert [] xs
    extractHeap [] = []
    extractHeap ts = (extractMin ts) : extractHeap (deleteMin ts)

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary = (foldl addBin []) . (map rank)
  where
    addBin [] n = 1 : (zeros n)
    addBin ns n = [1] ++ (zeros (n - (length ns))) ++ ns
    zeros n     = take n (repeat 0)

{-
binarySum :: [Int] -> [Int] -> [Int]
binarySum ns ns' = cr : sm
  where (sm, cr) = binarySum' ns ns'
binarySum' [] [] = ([], 0)
binarySum' ns [] = (ns, 0)
binarySum' [] ns = (ns, 0)
binarySum' (n:ns) (n':ns') = (s' : s, car')
  where
    (s', car') = adder n n' car
    (s, car) = binarySum' ns ns'

adder b1 b2 c = (xor (xor b1 b2) c, xor (bitAnd b1 b2) c)
xor b1 b2 = if (b1 /= b2) then 1 else 0
nand 1 1 = 0
nand _ _ = 1
bitAnd b1 b2 = 1 - (nand b1 b2)

sameNodes h h'
  = toBinary (mergeHeaps h h') == binarySum (toBinary h) (toBinary h')
-}

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]



