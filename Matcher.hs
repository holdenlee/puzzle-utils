{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XLambdaCase
#-}

module Matcher where
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
--import Text.Regex.Posix

--import Search

-- * Utils

lsing :: a -> [a]
lsing x = [x]

(.>=>) :: (Ord b, Ord c) => (a -> S.Set b) -> (b -> S.Set c) -> (a -> S.Set c)
(.>=>) f g x = (f x) .>>= g

(&) :: (Ord b, Ord c) => (a -> S.Set b) -> (b -> S.Set c) -> (a -> S.Set c)
(&) = (.>=>)

--S.unions $ map g $ S.toList (f x)

(.>>=) :: (Ord b, Ord c) => S.Set b -> (b -> S.Set c) -> (S.Set c)
(.>>=) s g = S.unions $ map g $ S.toList s

splits :: [a] -> [([a],[a])]
splits s = map (\n -> splitAt n s) [0..(length s)]

-- * Matcher

-- | From a string, give the set of possibilities for the remainder after the match. For example, matching any single character (`any`) would simply be a singleton containing the tail of the string, if the string is nonempty.
type Matcher a = [a] -> S.Set [a]

-- * Basic matching

ch :: (Eq a) => a -> Matcher a
ch c = str [c]

chs :: (Eq a, Ord a) => [a] -> Matcher a
chs li = strs (map lsing li)

chNot :: (Eq a) => [a] -> Matcher a
chNot li word = if not (null word) && not ((word!!0) `elem` li) then S.singleton $ tail word else S.empty
--drop (length s)

vowel :: Matcher Char
vowel = chs "aeiou"

consonant :: Matcher Char
consonant = chs "bcdfghjklmnpqrstvwxyz"

chrange :: (Ord a, Eq a, Enum a) => a -> a -> Matcher a
chrange x y = chs [x..y]

letter :: Matcher Char
letter = chrange 'a' 'z'

str :: (Eq a) => [a] -> Matcher a
str s word = if L.isPrefixOf s word 
             then S.singleton $ drop (length s) word
             else S.empty

--inefficient
strs :: (Ord a, Eq a) => [[a]] -> Matcher a
strs d word = S.fromList $ map snd $ filter ((`L.elem` d) . fst) $ splits word

strsRev :: (Ord a, Eq a) => [[a]] -> Matcher a
strsRev li = strs (map reverse li)

anyC :: Matcher a
anyC word = if null word then S.empty else S.singleton (tail word)

star :: (Ord a) => Matcher a
star word = S.fromList (L.tails word)

-- * Combinators

filterM :: (Ord a) => ([a] -> Bool) -> Matcher a -> Matcher a
filterM f m1 word = S.filter (\result -> f $ take ((length word) - (length result)) word) (m1 word)

repeatM :: (Ord a) => Int -> Matcher a -> Matcher a
repeatM n matcher = foldl1 (.>=>) (replicate n matcher)

zeroOrMore :: (Ord a) => Matcher a -> Matcher a
zeroOrMore matcher word = (S.singleton word) `S.union` ((matcher word) .>>= (zeroOrMore matcher))

oneOrMore :: (Ord a) => Matcher a -> Matcher a
oneOrMore matcher = matcher .>=> (zeroOrMore matcher)

(.|) :: (Ord a) => Matcher a -> Matcher a -> Matcher a
(.|) m1 m2 word = (m1 word) `S.union` (m2 word)

-- * Execution

matchP :: (Ord a) => Matcher a -> [[a]] -> [[a]]
matchP matcher dict = dict >>= (\word -> if [] `S.member` (matcher word) then [word] else [])
--S.null (matcher word) then [] else [word])

--compileRegex :: String -> Matcher a

{-
  Todo: Parse regex. Parse: .*a[ac][a-c][!abc](ab)^(ab)+@#><
-}
