{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XExistentialQuantification
 -XRank2Types
 -XFlexibleInstances
 -XFlexibleContexts
 -XTypeFamilies
 -XUndecidableInstances

#-}

{-# LANGUAGE TemplateHaskell #-}

module Search where
import qualified Data.Set as S
import qualified Data.Map as M
--import Control.Monad
--import Control.Monad.Trans.Except
--import Control.Monad.List
import Data.Maybe
import Data.Bifunctor
import Control.Lens hiding ((:<))
import Data.Sequence as Seq hiding (empty, null, length, filter, take, drop, splitAt, scanl)
import Data.Monoid
import Control.Monad
import Control.Monad.Trans.State
import Data.List
import qualified Data.MultiMap as MM
import Utilities
import Pointed

{-
For example, `m a` is a list, array, priority queue of elements of type `a`.
-}
class Poppable m where
  tryPop :: m a -> Maybe (a, m a)

instance Poppable Seq where
  tryPop s = case viewl s of
              h :< rest -> Just (h, rest)
              EmptyL -> Nothing

instance Poppable [] where
  tryPop li = case li of
               h:rest -> Just (h, rest)
               [] -> Nothing

--http://stackoverflow.com/questions/32927048/switch-order-of-arguments-for-instance-declaration-in-haskell/32927165#32927165

{-
`p` is priority (ex. Int, Float)
-}
newtype PQueue p a = PQueue {_mmap :: MM.MultiMap p a}

instance (Show p, Show a) => Show (PQueue p a) where
  show (PQueue q) = show $ MM.toMap q

instance (Pointed (PQueue p a)) where
  point = PQueue (MM.empty)

makeLenses ''PQueue

instance (Ord p) => Poppable (PQueue p) where
  tryPop (PQueue mp) =
    do
      ((prio, x:rest), mp2) <- M.minViewWithKey $ MM.toMap mp
      return (x, PQueue (mp & MM.delete prio
                            & foldIterate (MM.insert prio) rest))


{- 
Pop until something satisfies the predicate.
-}
popFirstTrue :: (Poppable m) => (a -> Bool) -> m a -> Maybe (a, m a)
popFirstTrue cond li = case tryPop li of
                        j@(Just (h, rest)) -> if cond h
                                              then j
                                              else popFirstTrue cond rest
                        n -> n  --Nothing
  
{-
How to initialize, how to insert a list, whether to check children for the predicate.
-}
data SearchMethod m a = SearchMethod {_insertS :: [a] -> m a -> m a,
                                      _checkChildren :: Bool}

initS :: (Pointed (m a)) => SearchMethod m a -> [a] -> m a
initS sm li = _insertS sm li point

makeLenses ''SearchMethod

{-
Given a search method, a function to give the children of every element, and a predicate, execute a search step. `Left` branch of Except when the search has finished with the element, or failure.
-}
searchStep :: Poppable m => SearchMethod m a -> (a -> [a]) -> (a -> Bool) -> [a] -> State (m a) [a]
--(Maybe [a]) 
searchStep (SearchMethod insert' checkChildren') f cond li =
  do
    start <- get 
    case tryPop start of
      Nothing -> return li
      Just (picked, rest) -> do
        let children = f picked
        put (insert' children rest)
        if checkChildren'
          then
              return $ (li ++ (filter cond children))
          else return $ (li ++ (filter cond [picked]))

search1' :: (Poppable m, Pointed (m a)) => SearchMethod m a -> (a -> [a]) -> (a -> Bool) -> m a -> ([a], m a)
search1' sm f cond = runState (untilM (\x -> do
                                         l <- get
                                         case tryPop l of
                                           Nothing -> return True
                                           _ -> return $ not $ null x) (searchStep sm f cond) [])

search1 :: (Poppable m, Pointed (m a)) => SearchMethod m a -> (a -> [a]) -> (a -> Bool) -> [a] -> Maybe a
search1 sm f cond start = listToMaybe $ fst $ search1' sm f cond (initS sm start)

--SearchMethod m a -> (a -> [a]) -> (a -> Bool) -> m a -> ([a], m a)
search' :: (Poppable m, Pointed (m a)) => SearchMethod m a -> (a -> [a]) -> (a -> Bool) -> m a -> [a]
search' sm f cond ma = case search1' sm f cond ma of
                           ([], _) -> [] --all options exhausted
                           (li, st) -> li++(search' sm f cond st)

search :: (Poppable m, Pointed (m a)) => SearchMethod m a -> (a -> [a]) -> (a -> Bool) -> [a] -> [a]
search sm f cond start = search' sm f cond (initS sm start)

dFS :: SearchMethod [] a
dFS = SearchMethod (++) False 

bFS' :: SearchMethod [] a
bFS' = SearchMethod (flip (++)) True

bFS :: SearchMethod Seq a
bFS = SearchMethod (\x y -> y <> (Seq.fromList x)) True

bestFS :: (Ord k) => (a -> k) -> Bool -> SearchMethod (PQueue k) a
bestFS heuristic findMin = SearchMethod (foldIterate (\x -> mmap %~ MM.insert (heuristic x) x))
                                        (not findMin)

{-
class IsPathOf a b | a -> b where
    ident :: a -> b

instance IsPathOf [a] a where
    ident = head
-}
-- _ident is repeated... :(
{-
data WithMemory m b = WithMemory {_mem ::S.Set b, _possibs ::m [b]}
--ex. a is a path, and b is the final node.

--need FlexibleContexts
instance (Show b, Show (m [b])) => Show (WithMemory m b) where
  show (WithMemory mem' possibs') = "Memory: "++(show mem') ++ "\nList: "++(show possibs')

makeLenses ''WithMemory

instance (Ord b, Poppable m) => Poppable (WithMemory m b) where
  tryPop wm@(WithMemory mem' possibs') =
    do
      -- (a, m a)
      (h,rest) <- popFirstTrue ((`S.notMember` mem') . head) possibs'
      return (h, WithMemory (S.insert (head h) mem') rest)

searchWithMemory :: (Ord b, Poppable m) => SearchMethod m a -> SearchMethod (WithMemory b m) a
searchWithMemory (SearchMethod insertS' checkChildren') =
  SearchMethod (\x -> possibs %~ (insertS' x))
               checkChildren'
-}
{-
bestFSMem :: (Ord b, Ord p) => (a -> b) -> (a -> p) -> SearchMethod (WithMemory b (PQueue p)) a
bestFSMem ident' heuristic = searchWithMemory ident' (bestFS heuristic True)

makePathFunction1 :: (a -> [(b, a)]) -> ([b], a) -> [([b], a)]
makePathFunction1 f (path, x) = map (\(edge, vert) -> (edge:path, vert)) $ f x -- [(b,a)]

makePathFunction2 :: (a -> [(b, a)]) -> ([(b,a)], a) -> [([(b,a)], a)]
makePathFunction2 f (path, x) = map (\(edge, vert) -> ((edge, vert):path, vert)) $ f x -- [(b,a)]

makePathFunction :: (a -> [a]) -> [a] -> [[a]]
makePathFunction f path = map (:path) $ f $ head path -- [(b,a)]

l1 :: (Num a) => (a, a) -> (a, a) -> a
l1 (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

obstacles = map (\x -> (x, 10-x)) [0..10]

bestFSEx = bestFSMem head (\path -> length path + l1 (10,10) (head path))

ss = searchStep bestFSEx
                (makePathFunction (\(x,y) -> filter (not . (`elem` obstacles)) [(x+1,y), (x-1, y), (x, y-1), (x,y+1)]))
                --filter (not . (`elem` obstacles)) 
                ((==(10,10)). head)

st = (_initS bestFSEx) [[(0,0)]]
st1 = ss st
alls = scanl (>>=) st1 (repeat ss)

ans = search (bestFSMem head (\path -> length path + l1 (10,10) (head path)))
             (makePathFunction (\(x,y) -> filter (not . (`elem` obstacles)) [(x+1,y), (x-1, y), (x, y-1), (x,y+1)]))
             ((==(10,10)). head)
             [[(0,0)]]
-}
{-
TODO:
* Square root search
* Instantiate for graph search
* (Give nice example.)
* Make paths
* Ex. searching on grid.
** f (x,y) = filter (not . obstacleAt) (map ($ (x,y)) [first (+1), first (-1), second (+1), second (-1)])
* Draw nice picture (with Helm).
* Alpha-beta search (this is different)
-}

{-

My first attempt is to switch it around with a data family declaration:

    data family PSQ' a b
    data instance PSQ' a b = PSQ b a
    instance (Ord p) => Poppable (PSQ' p) where
      tryPop = fmap (first Q.key) . Q.minView

However this gives the error
    
    Couldn't match type `Q.PSQ a p0' with `PSQ' p a'

even though they can match by setting p=p0.
-}
