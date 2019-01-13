import qualified Data.Map.Strict as M
import System.IO
import Data.Char
import Control.Monad
import Data.Foldable

cook :: M.Map String String -> String -> [(Int, String)] -> M.Map String String
cook d str ingreds = 
    let
        sublis = map (\(n, str) -> 
                          case str of 
                            "B" -> replicate n 'X'
                            "W" -> replicate n ' '
                            _ -> concat $ replicate n (d M.! str)) ingreds
        food = concat sublis
    in
      M.insert str food d

disp :: Int -> String -> String
disp n li = case li of
              [] -> ""
              _ -> (take n li)++"\n"++(disp n (drop n li))

parseWord :: String -> (Int, String)
parseWord str = (read (filter isDigit str), filter (not.isDigit) str)

parseRecipe :: String -> [(Int, String)]
parseRecipe str = map parseWord (lines str)

main = do
  let li = ["SM", "SS", "GR", "FS", "SF", "TC", "HP", "DC", "ST", "CC","1","2","3","4","5","6","7","8","Test"]
  d2 <- foldlM (\d filename -> do
                  txt <- readFile filename
                  return $ cook d filename (parseRecipe txt))
       M.empty li
  --putStrLn (disp 10 (d2 M.! "SM"))
  --forM_ (zip [10,10,10,10] ["SM","SS","GR","FS"]) (\(n,a) -> putStrLn (disp n (d2 M.! a)))
  forM_ li (\a -> putStrLn (show (length (d2 M.! a))))
  let l1 = [20, 32, 17, 19, 37, 33, 22, 27]
  let l2 = ["1","2","3","4","5","6","7","8"]
  forM_ (zip l1 l2)
      (\(n,a) -> putStrLn (disp n (d2 M.! a)))
  {-
  forM_ (zip [20, 32, 17, 19, 37, 33, 22, 27] ["1","2","3","4","5","6","7","8"])
      (\(n,a) -> putStrLn (disp n (d2 M.! a)))
  -}
--forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
