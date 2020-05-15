data Tree = Empty | Node Tree Integer Tree
    deriving (Show, Read, Eq)

--лист без поддеревьев
singleton :: Integer -> Tree
singleton x = Node  Empty x Empty

--Вставка элемента в дерево.
treeInsert :: Integer -> Tree  -> Tree 
treeInsert x Empty = singleton x
treeInsert x (Node left a right)
     | x == a = Node left x right
     | x < a  = Node (treeInsert x left) a  right
     | x > a  = Node left a (treeInsert x right)

--Поиск
treeElem :: Integer -> Tree  -> Bool
treeElem x Empty = False
treeElem x (Node left a right)
     | x == a = True
     | x < a  = treeElem x left
     | x > a  = treeElem x right

--Удаление
treeRemove :: Tree -> Integer -> Tree
treeRemove Empty _ = (Empty)
treeRemove (Node left x right) k                                       
   | k > x = Node left x (treeRemove right k)       
   | k < x = Node (treeRemove left k) x right
   | k == x =           
         if (left == Empty) && (right == Empty)  
             then Empty          
             else if (right == Empty) 
                     then left
                     else if (left == Empty)
                              then right
                              else if  (left /= Empty) && (right /= Empty)                                   
                                      then (Node left (treeMin right) (treeRemove right (treeMin right)))
                                      --ot greha podal'she
                                      else error "error!!!"

treeMin :: Tree -> Integer
treeMin tree  = head (treeList tree)

treeList :: Tree -> [Integer]
treeList Empty = []
treeList (Node left v right) = treeList left ++ [v] ++ treeList right