import qualified Data.List as L
import Data.Foldable
import Utilities

f1 x = x/6*5-24

f2 x = 8*x+17

f3 x = x^4-7

f4 x = x/7+1

f5 x = 8*(logBase 2 (x-50))

f6 x = x^3/7*6

f7 x = (x+29)/3+22

main = do
  let r = (map fromIntegral [1..807])
  let perms = L.permutations (zip [1..7] [f1,f2,f3,f4,f5,f6,f7])
  forM_ perms (\li -> do
                   --putStrLn (show (map fst li))
                   forM_ r (\n -> do
                      let res = foldl (|>) n (map snd li)
                      if abs(res-n)<0.001 then putStrLn (show ((map fst li), n)) else return ()))
                      --res==n
                       
  --fs <- L.permutations


