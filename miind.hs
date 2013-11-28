import Control.Monad.Writer

type Activity = Double
type Weight = Double

class Node n where
    evaluate :: n -> n

data SparseNode = SparseNode { 
      sparseNodeActivity :: Activity
    , sparseNodePredecessors :: [SparseNode]
    , sparseNodeWeights :: [Weight] 
} 

instance Show SparseNode where
   show (SparseNode a p w) = show a

instance Node SparseNode where
    evaluate (SparseNode a pl w) = SparseNode (foldl f 0 $ zip pl w) pl w
                                                   where f acc ((SparseNode a _ _), w) = a * w + acc 
--Architecture/ NetLib

-- Mit der Architecture bin ich mir noch nicht sicher, wie ich das Benutze und ob überhaupt!
data Architecture = [ANode] 
data ANode = ALeaf | ANode [ANode]

data Layer n = Layer [n] --TODO How can I restrict n to be instance of typeclass Node?
    deriving (Show)

instance Functor Layer where
    fmap f (Layer n) = Layer $ map f n

-- Test data

-- Ich beschriebe hier jetzt schon eine Implementation, nicht einer Architecture!


-- nodes
n1 = SparseNode 0.0 [SparseNode 1.0 [] [], SparseNode 4.0 [] [], SparseNode (-3) [] []] [2, -3, 1]
n2 = SparseNode 0.0 [] []
n3 = SparseNode 3.0 [] []
n4 = SparseNode 2.0 [] []
n5 = SparseNode 1.0 [] []

n6 = SparseNode 0.0 [n1, n2, n3] [1, -1, 3]
n7 = SparseNode 1.0 [n3] [3]
n8 = SparseNode (-1.0) [n3, n4, n5] [-1, 1, 0]

n9 = SparseNode 0.0 [n6, n7] [0.1, 1]
n10 = SparseNode 1 [n7, n8] [-1, 1]

-- layer
l1 = Layer [n1, n2, n3, n4, n5] 
l2 = Layer [n6, n7, n8]
l3 = Layer [n9, n10]



