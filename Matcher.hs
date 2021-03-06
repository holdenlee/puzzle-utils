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
import Data.Either
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

import Text.ParserCombinators.Parsec
--import qualified Text.Parsec.Token as P
--import Text.Parsec.Language (emptyDef)

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

(.>>=!) :: (Ord b, Ord c) => S.Set b -> (b -> S.Set c) -> (S.Set c)
(.>>=!) s g = S.unions $ S.toList $ S.map g s

splits :: [a] -> [([a],[a])]
splits s = map (\n -> splitAt n s) [0..(length s)]

{-
dict' :: IO (S.Set String)
dict' = do
  s <- readFile "words.txt"
  return $ S.fromList $ lines s
-}

-- * Matcher

-- | From a string, give the list of possibilities for the remainder after the match. For example, matching any single character (`any`) would simply be a singleton containing the tail of the string, if the string is nonempty.
type Matcher a = [a] -> S.Set [a]

-- * Basic matching

ch :: (Eq a) => a -> Matcher a
ch c = str [c]

chs :: (Eq a, Ord a) => [a] -> Matcher a
chs li = strs (S.fromList $ map lsing li)

chNot :: (Eq a) => [a] -> Matcher a
chNot li word = if not (null word) && not ((word!!0) `elem` li) then S.singleton $ tail word else S.empty
--drop (length s)

vowel :: Matcher Char
vowel = chs "aeiou"

consonant :: Matcher Char
consonant = chs "bcdfghjklmnpqrstvwxyz"

chrange :: (Ord a, Eq a, Enum a) => a -> a -> Matcher a
chrange x y = chs [x..y]

letterP :: Matcher Char
letterP = chrange 'a' 'z'

str :: (Eq a) => [a] -> Matcher a
str s word = if L.isPrefixOf s word 
             then S.singleton $ drop (length s) word
             else S.empty

strs :: (Ord a, Eq a) => S.Set [a] -> Matcher a
strs d word = S.fromList $ map snd $ filter ((`S.member` d) . fst) $ splits word

strsRev :: (Ord a, Eq a) => S.Set [a] -> Matcher a
strsRev li = strs (S.map reverse li)

anyC :: Matcher a
anyC word = if null word then S.empty else S.singleton (tail word)

star :: (Ord a) => Matcher a
star word = S.fromList (L.tails word)

-- * Combinators

filterP :: (Ord a) => ([a] -> Bool) -> Matcher a -> Matcher a
filterP f m1 word = S.filter (\result -> f $ take ((length word) - (length result)) word) (m1 word)

repeatP :: (Ord a) => Int -> Matcher a -> Matcher a
repeatP n matcher = foldl1 (.>=>) (replicate n matcher)

zeroOrMore :: (Ord a) => Matcher a -> Matcher a
zeroOrMore matcher word = (S.singleton word) `S.union` ((matcher word) .>>= (zeroOrMore matcher))

oneOrMore :: (Ord a) => Matcher a -> Matcher a
oneOrMore matcher = matcher .>=> (zeroOrMore matcher)

(.|) :: (Ord a) => Matcher a -> Matcher a -> Matcher a
(.|) m1 m2 word = (m1 word) `S.union` (m2 word)

-- * Execution

matchP :: (Ord a) => Matcher a -> S.Set [a] -> S.Set [a]
matchP matcher dict = S.fromDistinctAscList $ (S.toList dict) >>= (\word -> if [] `S.member` (matcher word) then [word] else [])
--S.null (matcher word) then [] else [word])

matchP' :: (Ord a) => Matcher a -> [[a]] -> [[a]]
matchP' matcher dict = dict >>= (\word -> if [] `S.member` (matcher word) then [word] else [])


-- * Parser

-- could also have done everything above with parsec? but I don't know how backtracking works in parsec.

type PM = Parser (Matcher Char)

insideBrackets :: Parser String
insideBrackets = try 
                 (do
                   ls <-many1 letter
                   char ']'
                   return ls
                 ) <|> 
                 (do 
                   char '!'
                   lets <- many1 letter
                   char ']'
                   return (['a'..'z'] L.\\ lets)
                 ) <|>
                 do {
                   beg <- letter;
                   char '-';
                   end <- letter;
                   char ']';
                   return [beg..end]
                 }

bracketedExpression :: PM
bracketedExpression = 
    do
      char '['
      set <- insideBrackets
      --char ']'
      let m = chs set
      (char '^' >> return (zeroOrMore m)) <|> (char '+' >> return (oneOrMore m)) <|> (return m)

parenExpression :: S.Set String -> PM
parenExpression d = 
    do
      char '('
      m <- expr d
      char ')'
      (char '^' >> return (zeroOrMore m)) <|> (char '+' >> return (oneOrMore m)) <|> (return m)

vowelE = char '@' >> return vowel
consonantE = char '#' >> return consonant 
dotE = char '.' >> return anyC
starE = char '*' >> return star
letterE = letter >>= (return . ch)

wordF :: S.Set String -> PM
wordF d = char '>' >> return (strs d)

wordR :: S.Set String -> PM
wordR d = char '<' >> return (strsRev d)
--how to do > and < ? make these 'd ->' instead.

expr :: S.Set String -> PM
expr d = do
  li <- many1 (letterE <|> dotE <|> starE <|> vowelE <|> consonantE <|> wordF d <|> wordR d <|> parenExpression d <|> bracketedExpression)
  return (foldl1 (&) li)

parseExpr :: S.Set String -> PM 
parseExpr d = do
  m <- expr d
  eof
  return m

compileRegex :: String -> S.Set String -> Matcher Char
compileRegex s d = fromRight (parse (expr d) "error" s)

matchRegex :: String -> S.Set String -> S.Set String
matchRegex s d = matchP (compileRegex s d) d

matchRegex' :: String -> S.Set String -> [String]
matchRegex' s d = matchP' (compileRegex s d) (S.toList d)


{-
  Todo: Parse regex. Parse: .*a[ac][a-c][!abc](ab)^(ab)+@#><
  mapMonotonic for lazy output for set
-}
{-
ch <- loadD "elements.txt"
c1=map (map toLower) ch
cd = S.fromList c1
  filter ((>=10).length)  $ matchP (zeroOrMore (strs cd)) dl
-}
