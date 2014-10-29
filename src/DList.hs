module DList where

data DList a = DLNode (DList a) a (DList a)
 
mkDList :: [a] -> DList a
mkDList [] = error "must have at least one element"
mkDList list = let (firstElem,lastElem) = go lastElem list firstElem
               in  firstElem
  where go :: DList a -> [a] -> DList a -> (DList a, DList a)
        go prev []     next = (next,prev)
        go prev (x:xs) next = let this        = DLNode prev x rest
                                  (rest,lastElem) = go this xs next
                              in  (this,lastElem)


goto::(Eq a) => DList a -> a -> DList a
goto this@(DLNode _left cur right) x | cur == x = this
                                     | otherwise = goto right x

apply::(a -> a) -> DList a -> DList a
apply f (DLNode left x right) = DLNode left (f x) right

{-
mkDList (1:(2:[])) ->

eerste = DLNode prev 1 rest1
rest1 = DLNode eerste 2 rest2
rest2 = go rest1 [] next = next
lastElem = rest1

dus : this = DLNode prev 1 rest1
lastElem = DLNode this 2 next

returning (this,lastElem) and assigning them to 
(next, prev)-}