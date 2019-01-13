{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Thunk where
import System.Environment
import Control.Monad
import Data.Tree
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Hashable
import Data.Maybe
import Debug.Trace
import qualified Data.Set as S
import Utilities
import Data.String.Utils
import Data.Char
import Data.Array as A

import Control.Lens
--import Algebra.Ring hiding ((^))
--import Prelude hiding ((*))

{-
a :: A.Array (Int, Int) Int
a = A.listArray ((0,0),(3,3)) [1..16]

b = a & (ix (3,3)) .~ 4

main = putStrLn $ show $ (a ^? ix (3,3))
-}

type Dir = (Int, Int)

isDone :: Int -> ((Int, Int), Int, Dir,  A.Array (Int, Int) Char) -> Bool
isDone dim (l@(x,y), t, d@(x1,y1), a) = 
    not (x>=1 && x<= dim && y>=1 && y <= dim)

mapp :: (a->b) -> (a,a) -> (b,b)
mapp f (x,y) = (f x, f y)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

runStep :: ((Int, Int), Int, Dir,  A.Array (Int, Int) Char) -> ((Int, Int), Int, Dir, A.Array (Int, Int) Char)
runStep (l@(x,y), t, d@(x1,y1), a) = 
    let 
        ((dx,dy), newT, newA) = 
            case a ^? ix (x,y) of 
              Just '/' -> (mapp ((-1)*) $ swap d, t+1, a & (ix l) .~ '\\') 
              Just '\\' -> (swap d, t+1, a & (ix l) .~ '/')
              _ -> (d, t, a)
    in
      ((x+dx,y+dy), newT, (dx,dy), newA)

doUntil :: (a -> Bool) -> (a -> a) -> a -> a
doUntil cond f x = 
    if cond x then x else doUntil cond f (f x)

translateInput :: Int -> Int -> Char -> a-> ((Int, Int), Int, Dir, a)
translateInput d x c a = 
    case c of
      'E' -> ((1,x), 0, (1,0), a)
      'W' -> ((d,x), 0, (-1,0), a)
      'S' -> ((x,1), 0, (0,1), a)
      'N' -> ((x,d), 0, (0,-1), a)

translateOutput :: Int -> Int -> Char ->(Int, Int)
translateOutput d x c = 
    case c of
      'E' -> (d+1,x)
      'W' -> (0,x)
      'S' -> (x,d+1)
      'N' -> (x,0)


evolve' :: Int -> Int -> Char -> A.Array (Int, Int) Char -> ((Int, Int), Int, Dir, A.Array (Int, Int) Char)
evolve' d x c a = doUntil (isDone d) runStep (translateInput d x c a)

evolve :: Int -> Int -> Char -> Int -> Int -> Char -> A.Array (Int, Int) Char -> [A.Array (Int, Int) Char]
evolve d x c th xf cf a = 
    let
        ((x0,y0), t, (x1,y1), a') = doUntil (isDone d) runStep (translateInput d x c a)
    in
        if th==t && (x0,y0) == translateOutput d xf cf then [a'] else []

elim :: Int -> Int -> Char -> Int -> Int -> Char -> [A.Array (Int, Int) Char] -> [A.Array (Int, Int) Char] 
elim d x c th xf cf as = as >>= (evolve d x c th xf cf)

bulkElim :: Int -> [(Int, Char, Int, Int, Char)] -> [A.Array (Int, Int) Char] -> [A.Array (Int, Int) Char] 
bulkElim dim li a0 = foldl (\l (a,b,c,d,e) -> elim dim a b c d e l) a0 li
    
listPower :: Int -> [a] -> [[a]]
listPower t li = 
    if t == 0 then [[]] else (listPower (t-1) li) >>= (\x -> map (\y -> x++[y]) li)

makeBoxes :: Int -> [A.Array (Int, Int) Char]
makeBoxes d = map (A.listArray ((1,1),(d,d))) (listPower (d^2) "/\\ ")

makeBoxes2 :: Int -> [Char] -> [A.Array (Int, Int) Char]
makeBoxes2 d li = map (A.listArray ((1,1),(d,d))) (makeBoxes2' li)

makeBoxes2' :: [Char] -> [[Char]]
makeBoxes2' li = case li of
                  [] -> [[]]
                  '1':rest -> (makeBoxes2' rest) >>= (\x -> [' ':x])
                  '2':rest -> (makeBoxes2' rest) >>= (\x -> ['/':x, '\\':x])
                  '/':rest -> (makeBoxes2' rest) >>= (\x -> ['/':x])
                  'l':rest -> (makeBoxes2' rest) >>= (\x -> ['\\':x])
                  '3':rest -> (makeBoxes2' rest) >>= (\x -> ['/':x, '\\':x, ' ':x])
                  
                  
