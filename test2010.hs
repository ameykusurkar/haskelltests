data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (s:ss) (t:ts) = (s == t) && (isPrefix ss ts)

removePrefix :: String -> String -> String
--Pre: s is a prefix of s'
removePrefix s = drop (length s)

suffixes :: [a] -> [[a]]
suffixes ls = take (length ls) (iterate tail ls)

isSubstring :: String -> String -> Bool
isSubstring s t = any (isPrefix s) (suffixes t)

findSubstrings :: String -> String -> [Int]
findSubstrings s t = ((map snd) . (filter (id.fst))) (zip isSubList [0..])
  where isSubList = (map (isPrefix s) (suffixes t))

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf n)     = [n]
getIndices (Node nodes) = concatMap (getIndices . snd) nodes

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition l1 l2  = part l1 l2 []
  where
    part [] ys pref = (pref, [], ys)
    part xs [] pref = (pref, xs, [])
    part (x:xs) (y:ys) pref
      | x /= y    = (pref, (x:xs), (y:ys))
      | otherwise = part xs ys (pref++[x])

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf n) = [n]
findSubstrings' _  (Leaf _) = []
findSubstrings' ss (Node nodes) = concat (map getIndex nodes)
  where
    getIndex (str, tree)
      | ss' == ""  = getIndices tree
      | str' == "" = findSubstrings' ss' tree
      | otherwise  = []
      where (pre, ss', str') = partition ss str

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node []) = Node [(s, Leaf n)]
insert (s, n) nd@( Node ((str, tree):nodes) )
  | pre == ""  = addNode (str, tree) (insert (s, n) (Node nodes))
  | pre == str = Node ((str, insert (remS, n) tree) : nodes)
  | otherwise  = Node ([(pre, Node [(remS, Leaf n), (remStr, tree)])]++nodes)
    where 
      (pre, remS, remStr) = partition s str
      addNode nd (Node nds) = Node (nd:nds)

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..length s-1])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


