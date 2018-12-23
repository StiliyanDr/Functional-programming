module DataTypes where
import Prelude hiding (head, tail)

data Natural = Zero | Successor Natural
               deriving (Eq, Ord, Show, Read)

fromNatural :: Natural -> Integer
fromNatural Zero = 0
fromNatural (Successor natural) = 1 + fromNatural natural

toNatural :: Integer -> Natural
toNatural 0 = Zero
toNatural number
 | number > 0 = Successor (toNatural (number - 1))
 | otherwise = error "Natural numbers are nonnegative!"

data PositiveBinaryNumber = One
                            | BitZero PositiveBinaryNumber
			    | BitOne PositiveBinaryNumber
			    deriving (Eq, Ord, Show, Read)

toDecimal :: PositiveBinaryNumber -> Integer
toDecimal One = 1
toDecimal (BitZero binaryNumber) = (toDecimal binaryNumber) * 2
toDecimal (BitOne binaryNumber) = (toDecimal binaryNumber) * 2 + 1

increment :: PositiveBinaryNumber -> PositiveBinaryNumber
increment One = BitZero One
increment (BitZero binary) = BitOne binary
increment (BitOne binary) = BitZero (increment binary)

data List item = EmptyList | List { head :: item,
                                    tail :: List item }
                 deriving (Eq, Ord, Show)

fromList :: List item -> [item]
fromList EmptyList = []
fromList list = (head list : fromList (tail list))

toList :: [item] -> List item
toList [] = EmptyList
toList (h : t) = List h (toList t)

append :: List item -> List item -> List item
append EmptyList rhs = rhs
append (List h t) rhs = List h (append t rhs)

data BinaryTree item = EmptyTree | BinaryTree { root :: item,
                                                leftSubtree :: BinaryTree item,
						rightSubtree :: BinaryTree item }
		                                deriving (Eq, Ord, Show)

isEmptyTree :: BinaryTree item -> Bool
isEmptyTree EmptyTree = True
isEmptyTree _ = False

makeLeaf :: item -> BinaryTree item
makeLeaf item = BinaryTree item EmptyTree EmptyTree

isLeaf :: BinaryTree item -> Bool
isLeaf (BinaryTree _ EmptyTree EmptyTree) = True
isLeaf _ = False

depthOf :: BinaryTree item -> Integer
depthOf EmptyTree = 0
depthOf (BinaryTree _ left right) = 1 + max (depthOf left) (depthOf right)

leavesOf :: BinaryTree item -> [item]
leavesOf EmptyTree = []
leavesOf (BinaryTree root EmptyTree EmptyTree) = [root]
leavesOf (BinaryTree root left right) = leavesOf left ++ leavesOf right

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ EmptyTree = EmptyTree
mapTree function (BinaryTree root leftSubtree rightSubtree) =
 BinaryTree (function root)
            (mapTree function leftSubtree)
	    (mapTree function rightSubtree)

foldRightBinaryTree :: (item -> r -> r) -> r -> BinaryTree item -> r
foldRightBinaryTree _ neutralElement EmptyTree = neutralElement
foldRightBinaryTree operation neutralElement (BinaryTree root leftSubtree rightSubtree) =
 foldRightBinaryTree operation subresult leftSubtree
  where subresult = operation root resultInRightSubtree
        resultInRightSubtree = foldRightBinaryTree operation neutralElement rightSubtree

foldLeftBinaryTree :: (r -> item -> r) -> r -> BinaryTree item -> r
foldLeftBinaryTree _ neutralElement EmptyTree = neutralElement
foldLeftBinaryTree operation neutralElement (BinaryTree root leftSubtree rightSubtree) =
 foldLeftBinaryTree operation (operation resultInLeftSubtree root) rightSubtree
  where resultInLeftSubtree = foldLeftBinaryTree operation neutralElement leftSubtree

foldLeftBinaryTree1 :: (r -> r -> r) -> BinaryTree r -> r
foldLeftBinaryTree1 operation (BinaryTree root EmptyTree rightSubtree) =
 foldLeftBinaryTree operation root rightSubtree
foldLeftBinaryTree1 operation (BinaryTree root leftSubtree rightSubtree) =
 foldLeftBinaryTree operation (operation resultInLeftSubtree root) rightSubtree
  where resultInLeftSubtree = foldLeftBinaryTree1 operation leftSubtree

foldRightBinaryTree1 :: (r -> r -> r) -> BinaryTree r -> r
foldRightBinaryTree1 operation (BinaryTree root leftSubtree EmptyTree) =
 foldRightBinaryTree operation root leftSubtree
foldRightBinaryTree1 operation (BinaryTree root leftSubtree rightSubtree) =
 foldRightBinaryTree operation (operation root resultInRightSubtree) leftSubtree
  where resultInRightSubtree = foldRightBinaryTree1 operation rightSubtree

bstree :: BinaryTree Int
bstree = BinaryTree { root = 5,
                      leftSubtree = BinaryTree { root = 3,
		                                 leftSubtree = makeLeaf 2,
				                 rightSubtree = makeLeaf 4 },
		      rightSubtree = BinaryTree { root = 7,
		                                  leftSubtree = makeLeaf 6,
				                  rightSubtree = makeLeaf 8 } }

binaryTree :: BinaryTree Int
binaryTree = BinaryTree { root = 1,
                          leftSubtree = makeLeaf 2,
			  rightSubtree = makeLeaf 3 }

sumOfTree :: Num a => BinaryTree a -> a
sumOfTree = foldLeftBinaryTree (+) 0

productOfTree :: Num a => BinaryTree a -> a
productOfTree = foldRightBinaryTree (*) 1

maxSumOfPath :: (Num a, Ord a) => BinaryTree a -> a
maxSumOfPath EmptyTree = 0
maxSumOfPath (BinaryTree root leftSubtree rightSubtree) =
 root + max (maxSumOfPath leftSubtree) (maxSumOfPath rightSubtree)

prune :: BinaryTree item -> BinaryTree item
prune EmptyTree = EmptyTree
prune (BinaryTree _ EmptyTree EmptyTree) = EmptyTree
prune (BinaryTree root leftSubtree rightSubtree) =
 BinaryTree root (prune leftSubtree) (prune rightSubtree)

bloomWith :: (a -> a) -> BinaryTree a -> BinaryTree a
bloomWith _ EmptyTree = EmptyTree
bloomWith function (BinaryTree root EmptyTree EmptyTree) = BinaryTree root leaf leaf
 where leaf = makeLeaf (function root)
bloomWith function (BinaryTree root leftSubtree rightSubtree) =
 BinaryTree root
            (bloomWith function leftSubtree)
	    (bloomWith function rightSubtree)

bloom :: BinaryTree item -> BinaryTree item
bloom = bloomWith id

rotateLeft :: BinaryTree item -> BinaryTree item
rotateLeft EmptyTree = EmptyTree
rotateLeft tree@(BinaryTree _ _ EmptyTree) = tree
rotateLeft (BinaryTree root leftSubtree (BinaryTree rootOfRightSubtree
                                                    leftSubtreeOfRightSubtree
						    rightSubtreeOfRightSubtree)) =
 BinaryTree rootOfRightSubtree
            (BinaryTree root leftSubtree leftSubtreeOfRightSubtree)
	    rightSubtreeOfRightSubtree

rotateRight :: BinaryTree item -> BinaryTree item
rotateRight EmptyTree = EmptyTree
rotateRight tree@(BinaryTree _ EmptyTree _) = tree
rotateRight (BinaryTree root
                        (BinaryTree rootOfLeftSubtree
			            leftSubtreeOfLeftSubtree
				    rightSubtreeOfLeftSubtree)
		        rightSubtree) =
 BinaryTree rootOfLeftSubtree
            leftSubtreeOfLeftSubtree
	    (BinaryTree root
	                rightSubtreeOfLeftSubtree
			rightSubtree)


-- Binary search trees --

insertInBST :: Ord item => item -> BinaryTree item -> BinaryTree item
insertInBST item EmptyTree = makeLeaf item
insertInBST item (BinaryTree root leftSubtree rightSubtree) =
  if (item < root)
  then BinaryTree root (insertInBST item leftSubtree) rightSubtree
  else BinaryTree root leftSubtree (insertInBST item rightSubtree)

containsBST :: Ord item => item -> BinaryTree item -> Bool
containsBST item EmptyTree = False
containsBST item (BinaryTree root leftSubtree rightSubtree) =
 item == root || containsBST item (if item < root then leftSubtree else rightSubtree)

bstToList :: BinaryTree item -> [item]
bstToList EmptyTree = []
bstToList (BinaryTree root leftSubtree rightSubtree) = left ++ (root : right)
 where left = bstToList leftSubtree
       right = bstToList rightSubtree

nodesCountOf :: BinaryTree item -> Integer
nodesCountOf EmptyTree = 0
nodesCountOf (BinaryTree _ leftSubtree rightSubtree) =
 1 + nodesCountOf leftSubtree + nodesCountOf rightSubtree

listToBST :: Ord item => [item] -> BinaryTree item
listToBST items = foldr insertInBST EmptyTree items

bstSort :: Ord item => [item] -> [item]
bstSort = bstToList . listToBST

removeFromBST :: Ord item => item -> BinaryTree item -> BinaryTree item
removeFromBST _ EmptyTree = EmptyTree
removeFromBST item (BinaryTree root leftSubtree rightSubtree)
 | item < root = BinaryTree root (removeFromBST item leftSubtree) rightSubtree
 | item > root = BinaryTree root leftSubtree (removeFromBST item rightSubtree)
 | isEmptyTree leftSubtree = rightSubtree
 | otherwise = BinaryTree (maxInBST leftSubtree)
                          (removeMax leftSubtree)
			  rightSubtree

maxInBST :: BinaryTree item -> item
maxInBST (BinaryTree root _ EmptyTree) = root
maxInBST (BinaryTree _ _ rightSubtree) = maxInBST rightSubtree

removeMax :: BinaryTree item -> BinaryTree item
removeMax EmptyTree = EmptyTree
removeMax (BinaryTree _ leftSubtree EmptyTree) = leftSubtree
removeMax (BinaryTree root leftSubtree rightSubtree) =
 BinaryTree root leftSubtree (removeMax rightSubtree)

-- Associative memory, MAP --

data KeyValue key value = KeyValue { keyOf :: key, valueOf :: value }

type Map key value = BinaryTree (KeyValue key value)

insert :: Ord key => key -> value -> Map key value -> Map key value
insert key value EmptyTree = makeLeaf (KeyValue key value)
insert key value (BinaryTree root leftSubtree rightSubtree)
 | key == keyOf root = BinaryTree (KeyValue key value) leftSubtree rightSubtree
 | key < keyOf root = BinaryTree root (insert key value leftSubtree) rightSubtree
 | otherwise = BinaryTree root leftSubtree (insert key value rightSubtree)

search :: Ord key => key -> Map key value -> Maybe value
search _ EmptyTree = Nothing
search key (BinaryTree root leftSubtree rightSubtree)
 | key == keyOf root = Just (valueOf root)
 | key < keyOf root = search key leftSubtree
 | otherwise = search key rightSubtree

elementsCount :: Map key value -> Integer
elementsCount = nodesCountOf

isEmpty :: Map key value -> Bool
isEmpty = (== 0) . elementsCount

valuesOf :: Map key value -> [value]
valuesOf = map valueOf . bstToList

keysOf :: Map key value -> [key]
keysOf = map keyOf . bstToList

m :: Map Int String
m = BinaryTree { root = (KeyValue 5 "five"),
                 leftSubtree = BinaryTree { root = (KeyValue 3 "three"),
		                            leftSubtree = EmptyTree,
				            rightSubtree = EmptyTree },
		 rightSubtree = BinaryTree { root = (KeyValue 7 "seven"),
		                             leftSubtree = EmptyTree,
				             rightSubtree = EmptyTree } }

