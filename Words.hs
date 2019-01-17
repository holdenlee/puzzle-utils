{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XLambdaCase
#-}

module Words where
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
import Matcher

import Search

-- and filter
af :: (IO [String]) -> (String -> Bool) -> IO [String]
af iod f = fmap (filter f) iod

-- |Whether two strings are anagrams.
anagram :: String -> String -> Bool
anagram s1 s2 = (L.sort s1) == (L.sort s2)

sandwich :: String -> String -> IO [String]
sandwich w1 w2 = do
  d <- fmap S.toList dict
  return $ ((map (drop (length w1)) $ filter (\x -> w1 `L.isPrefixOf` x) d)) `L.intersect` ((map (dropLast (length w2)) $ filter (\x -> w2 `L.isSuffixOf` x) d))

-- |Computes Hamming distance between 2 strings. If different length, returns 9999.
hamming :: String -> String -> Int
hamming s1 s2 = case (s1, s2) of
                  ([], []) -> 0
                  (a:r1, b:r2) -> ((hamming r1 r2)+(if a==b then 0 else 1))
                  _ -> 9999

-- |Computes Hamming distance between 2 strings. If different length, returns None.
hammingM :: String -> String -> Maybe Int
hammingM s1 s2 = case (s1, s2) of
                   ([], []) -> Just 0
                   (a:r1, b:r2) -> do
                                h <- hammingM r1 r2
                                return (h+(if a==b then 0 else 1))
                   _ -> Nothing

matchQs :: String -> String -> Bool
matchQs s1 s2 = case (s1, s2) of
                  ([], []) -> True
                  (a:r1, b:r2) -> if a=='?' || a==b then matchQs r1 r2 else False
                  _ -> False

expandRE :: String -> String
expandRE = (\x -> "^"++x++"$") . (replace "*" "[a-z]*") . (replace "?" "[a-z]")

matches :: String -> String -> Bool
matches pat = (=~ (expandRE pat))
--filter (=~ "(foo|bar)")

dropLast :: Int -> [a] -> [a]
dropLast n li = take ((length li) - n) li

dict :: IO (S.Set String)
dict = loadD "words.txt"

loadD :: String -> IO (S.Set String)
loadD d = do
  s <- readFile d
  return $ S.fromList $ lines s

letterToInt :: Char -> Int
letterToInt c = (ord c) - 64

intToLetter :: Int -> Char
intToLetter i = (chr (i+64))

removeSpace :: String -> String
removeSpace = replace " " ""

getWordDifference :: String -> String -> String
getWordDifference a b = map intToLetter $ map2 (\x y -> ((ord x)-(ord y)+1) `mod` 26) (removeSpace a) (removeSpace b)

subWords :: String -> String -> String
subWords a b = map intToLetter $ map2 (\x y -> ((ord x)-(ord y)+1) `mod` 26) a b

--not efficient - should do searchM or something

combWords d lis = search dFS (\x -> filter (\y -> not $ null $ filter (y `L.isPrefixOf`) d) $ map (\i -> x++[i]) (if length x >= length lis then [] else lis!!(length x))) (\x -> length x == length lis && x `elem` d) [""]

--combWords d ["hd","oo","tg"]
--["hot","hog","hot","hog","dot","dog","dot","dog"]

cws :: [String] -> String -> Bool
cws lis str = (all (uncurry L.elem) (zip str lis)) && ((length lis)==(length str))

get2s :: Char -> String
get2s c = case c of 
             'a' -> "ceiq"	
             'b' -> "cfjr"	
             'c' -> "abgks"	
             'd' -> "eflt"	
             'e' -> "adgmu"	
             'f' -> "bdgnv"	
             'g' -> "cefow"	
             'h' -> "ijlx"	
             'i' -> "ahkmy"	
             'j' -> "bhknz"	
             'k' -> "cijo"	
             'l' -> "dhmn"	
             'm' -> "eilo"	
             'n' -> "fjlo"	
             'o' -> "gkmn"	
             'p' -> "qrtx"	
             'q' -> "apsuy"	
             'r' -> "bpsvz"	
             's' -> "cqrw"	
             't' -> "dpuv"	
             'u' -> "eqtw"	
             'v' -> "frtw"	
             'w' -> "gsuv"	
             'x' -> "hpyz"	
             'y' -> "iqx"	
             'z' -> "jrx"	

--map (\n -> toLower $ intToLetter $ f26 $ (n + (letterToInt $ toUpper $ c)) `mod` 26) [1,2,4,8,16,-1,-2,-4,-8,-16]

f26 :: Int -> Int
f26 x = case x of
          0 -> 26
          _ -> x

makeCands :: String -> String -> [String]
makeCands instr s = 
    (zip instr s) >>= (\(i,c) -> case i of
              'y' -> [get2s c]
              'r' -> []
              'w' -> [[c]]
              'g' -> [[c],[c]])

az :: String
az = ['a'..'z']

makeCands2' :: String -> String -> Maybe [String]
makeCands2' instr s = case (instr,s) of
                     ("", []) -> Just []
                     ('y':rest,h:rest') -> fmap ((get2s h):) (makeCands2' rest rest')
                     ('r':rest,_) -> fmap (az:) (makeCands2' rest s)
                     ('w':rest,h:rest') -> fmap ([h]:) (makeCands2' rest rest')
                     ('g':rest,h:j:rest') -> if h==j then fmap ([h]:) (makeCands2' rest rest') else Nothing
                     _ -> Nothing

makeCands2 :: String -> String -> [String]
makeCands2 instr s = case makeCands2' instr s of
                       Just li -> li
                       Nothing -> [""]
                                         

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . L.group . L.sort

transPossibs :: [String] -> String -> String -> [String]
transPossibs d instr s = filter (cws $ makeCands instr s) d
--remove duplicates?
--rmdups $ 
seqTrans :: [String] -> [String] -> String -> [String]
seqTrans d trans start = foldl (\xs tran -> xs >>= (transPossibs d tran)) [start] trans

transPossibsR :: [String] -> String -> String -> [String]
transPossibsR d instr s = filter (cws $ makeCands2 instr s) d

seqTransR :: [String] -> [String] -> String -> [String]
seqTransR d trans start = foldl (\xs tran -> xs >>= (transPossibsR d tran)) [start] trans

seqTransRL :: [String] -> [String] -> [String] -> [String]
seqTransRL d trans starts = foldl (\xs tran -> xs >>= (transPossibsR d tran)) starts trans

main :: IO ()
main = do
  d <- dict
  --let d = S.toList d'
  let l = map (\s -> matchRegex s d) ["l.......v",
                                  "..i[sz]e",
                                  "[l-p].[m-r].[w-z]",
                                  "#@#@#@#@#@#@#@",
                                  "xo*",
                                  "x*a",
                                  "*xj*",
                                  ".j*k*",
                                  "*a*e*i*o*u*",
                                  "*@@@@*",
                                  "ace>",
                                  "hi<g"
                                 ]
  putStrLn (show l)


{-seqTrans2 :: [String] -> [String] -> String -> [[String]]
seqTrans2 d trans start = foldl (\xs tran -> map (\x -> [x]) ((last xs) >>= (transPossibs d tran))) [[start]] trans-}

{-
repeatN :: Int -> (a -> a) -> a -> a
repeatN n f = if n==0 then id else (f . (repeatN (n - 1) f))

next :: (String, [String] , String) -> [(String, [String], String)]
next (cur, li, s) = 
    case li of
      n:rest -> (map (\x -> (cur++[x], rest, s)) n) ++ 
                (map (\i -> (cur++[s!!i], rest, (take i s)++(drop (i+1) s))) [0..((length s)-1)])
      _ -> if s==[] then [(cur, li, s)] else []

getAll :: ([String], String) -> [String]
getAll (li, s) = map (\(x,y,z) -> x) $ repeatN (1 + (length li)) (>>= next) [("", li, s)]

combWords li = do
  d <- dict
  let words = S.fromList d
  let li = getAll (li, "hot")
  let f = filter (\x -> S.member x words) li
  putStrLn (show f)
-}
{-
combWord :: [String] -> [String] -> [String] -> [[String]]
combWord d li x = x++
-}
{-
repeatN :: Int -> (a -> a) -> a -> a
repeatN n f = if n==0 then id else (f . (repeatN (n - 1) f))

next :: (String, [String] , String) -> [(String, [String], String)]
next (cur, li, s) = 
    case li of
      n:rest -> (map (\x -> (cur++[x], rest, s)) n) ++ 
                (map (\i -> (cur++[s!!i], rest, (take i s)++(drop (i+1) s))) [0..((length s)-1)])
      _ -> if s==[] then [(cur, li, s)] else []

getAll :: ([String], String) -> [String]
getAll (li, s) = map (\(x,y,z) -> x) $ repeatN (1 + (length li)) (>>= next) [("", li, s)]
  
main = do
  s <- readFile "words.txt"
  let words = S.fromList $ lines s
  let li = getAll (["gno","en","stt","is","hip"], "hot")
  let f = filter (\x -> S.member x words) li
  putStrLn (show f)

solve :: [String] -> String -> IO ()
solve x y = do
  s <- readFile "words.txt"
  let words = S.fromList $ lines s
  let li = getAll (x, y)
  let f = filter (\x -> S.member x words) li
  putStrLn (show f)
-}

{-
d <-dict 
seqTrans d ["rryyyrwwwwwww","yyyrrryyyw","yyyryyw","ywyywg"]
seqTrans d ["rryyyrwwwwwww","yyyrrryyyw","yyyryyw","ywyywg","yyyyyyr"] "technologists"

seqTrans d ["yyryryryyr","yryyyr","ygyy","ywyyy"] "laboratory"

seqTrans d ["wwwwyywrrryy","ywrwrywww","yyyyrrr"] "recreational"

seqTrans d ["wwwywy","yygrwy","yyyyyr"] "misfit"


seqTrans d ["yyyyyrrwyyrrww","rryyyyryrw","yyyywy","yyyyww"] "mathematicians"
-}
