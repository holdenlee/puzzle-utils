module SearchTest where

import Search

test1 = search1 dFS (\x -> [2*x, 2*x+1]) (==2) [1]

test = search dFS (\x -> filter (<1000) [2*x, 2*x+1]) (\x -> x==2 || x==5 || x==999) [1]
