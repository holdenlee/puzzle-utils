{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XLambdaCase
#-}

module Robber where
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
import Text.Regex.Posix

import Search

pedges :: M.Map Int Int
pedges = M.fromList 
        [(2,[1,4,3,18,28,30]),--28,30
         (4,[2,1,11,6,5,4]),
         (5,[3,4,6,7,17,19]),
         (7,[5,6,8,16,20,17]),
         (8,[6,11,10,9,16,7]),
         (9,[8,10,13,12,21,16]), --21
         (11,[6,4,1,14,10,8]),
         (13,[9,10,14,15,32,12]), --32
         (14,[10,11,1,30,15,13]), --1 ,30
         (18,[2,3,19,23,26,28]),
         (19,[3,5,17,22,23,18]),
         (20,[7,16,21,24,22,17]),
         (21,[9,12,27,24,20,16]),
         (22,[17,20,24,25,22,19]),
         (25,[22,24,27,29,26,23]),
         (26,[18,22,25,29,31,28]),
         (27,[21,12,32,29,25,24]),
         (30,[28,31,15,14,2,1]), --15...
         (31,[26,29,32,15,30,28]),
         (32,[27,0,0,0,31,29])]

getPEdges i = map (i,) (pedges M.! i)

pkeys = M.keys pedges

pents :: [String]
pents = ["PELE ","ERAS ","CRAM ","MITE ","LITRE","SATIN","CILER","ONIMA","AREPO","ARTET"," ETIC"," STAP"]

isValid :: (M.Map (Int,Int) Char, M.Map Int String, [String], [String]) -> Bool
isValid (hdict, pdict, hexes, pents) = 
    all (\(pn, hn) -> (not (M.member hn hdict)) || L.elem (pdict M.! (pn, hn)) (hdict M.! hn)) (M.keys pdict)

getChildren :: (M.Map (Int,Int) Char, M.Map Int String, [String], [String], Int) -> Int -> [(M.Map (Int,Int) Char, M.Map Int String, [String], [String], Int)]
getChildren (hdict, pdict, hexes, pents) i = 
    let
        

getChildrenI :: (M.Map (Int,Int) Char, M.Map Int String, [String], [String]) -> Int -> [(M.Map (Int,Int) Char, M.Map Int String, [String], [String])]
getChildrenI (hdict, pdict, hexes, pents) i =
  filter isValid $ 
    if not $ L.elem i pkeys 
    then -- hexagon
        map (\x -> 
                 let 
                     hdict' = M.insert i x hdict
                     hexes' = L.delete x hexes
                 in
                   (hdict', pdict, hexes', pents)) hexes
    else --pentagon
        map (\(r,x) -> 
                 let 
                     pdict' = pdict |> foldIterate2 (M.insert) (getPEdges i) (rotateL r x)
                     pents = L.delete (r,x) pents 
                 in
                   (hdict, pdict', hexes, pents'))

rotateL :: Int -> [a] -> [a] 
rotateL n li = (drop n li)++(take n li)

--(,) <$> [1,2,3] <*> [1,2,3]

combWords d lis = search dFS (\x -> filter (\y -> not $ null $ filter (y `L.isPrefixOf`) d) $ map (\i -> x++[i]) (if length x >= length lis then [] else lis!!(length x))) (\x -> length x == length lis && x `elem` d) [""]
