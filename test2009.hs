import Data.List
import Data.Maybe

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key xs = fromJust (lookup key xs)


checkSat :: BDD -> Env -> Bool
checkSat bdd env
  | nodeID < 2 = intToBool nodeID
  | otherwise  = checkSat' nodeID
  where
    (nodeID, nodes) = bdd
    checkSat' 0 = False
    checkSat' 1 = True
    checkSat' n
      | lookUp ind env = checkSat' rn
      | otherwise      = checkSat' ln
      where (ind, ln, rn) = lookUp n nodes

intToBool :: Int -> Bool
-- Pre: n < 2
-- Post: Returns 0 as False, 1 as True
intToBool 0 = False
intToBool 1 = True


sat :: BDD -> [[(Index, Bool)]]
sat (0, _) = []
sat bdd = sat' nodeID
  where
    (nodeID, nodes) = bdd
    sat' 0 = []
    sat' 1 = [[]]
    sat' n = satL ++ satR
      where
        satL = map ((ind, False):) (sat' ln)
        satR = map ((ind, True):) (sat' rn)
        (ind, ln, rn) = lookUp n nodes


------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim b)) 
  = Prim (not b)
simplify (Not e)
  = Not e
simplify (And (Prim True) (Prim True))
  = Prim True
simplify (And (Prim _) (Prim _))
  = Prim False
simplify (And e1 e2)
  = And e1 e2
simplify (Or (Prim False) (Prim False))
  = Prim False
simplify (Or (Prim _) (Prim _))
  = Prim True
simplify (Or e1 e2)
  = (Or e1 e2)

restrict :: BExp -> Index -> Bool -> BExp
restrict (IdRef n) ind b
  | ind == n  = Prim b
  | otherwise = IdRef n
restrict (Prim b) _ _ 
  = Prim b
restrict (Not bexp) ind b
  = simplify (Not (restrict bexp ind b))
restrict (And bexp1 bexp2) ind b
  = simplify (And (restrict bexp1 ind b) (restrict bexp2 ind b))
restrict (Or bexp1 bexp2) ind b
  = simplify (Or (restrict bexp1 ind b) (restrict bexp2 ind b))

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs = buildBDD' e 2 xs

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim b) n [] = (boolToInt b, [])
buildBDD' bexp n (x:xs) = (n, currNode:(lns ++ rns))
  where
    currNode = ( n, (x, idl, idr) )
    (idl, lns) = buildBDD' (restrict bexp x False) (2*n) xs
    (idr, rns) = buildBDD' (restrict bexp x True) (2*n + 1) xs

boolToInt :: Bool -> Int
-- Returns False as 0, True as 1
boolToInt False = 0
boolToInt True  = 1

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD 
  = undefined

sameSubtree :: BExp -> Index -> Bool
-- Returns True if the expression returns the same substrees
-- for both False and True
sameSubtree e ind = (ln == rn)
  where
    ln = restrict e ind False
    rn = restrict e ind True

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


