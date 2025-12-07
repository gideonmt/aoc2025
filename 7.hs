import System.IO
import qualified Data.Set as S
import qualified Data.Map as M

-- define output type of parse
type Item = String

sx :: [String] -> Int
sx g = head [x | (x, c) <- zip [0..] (head g), c == 'S']

p1 :: [Item] -> Int
p1 g = go S.empty [(sx g, 0)]
  where
    go s [] = S.size s
    go s ((x,y):bs) | S.member (x,y) s || y >= length g = go s bs
                    | g!!y!!x == '^' = go (S.insert (x,y) s) ((x-1,y+1):(x+1,y+1):bs)
                    | otherwise = go s ((x,y+1):bs)

p2 :: [Item] -> Int
p2 g = foldl f (M.fromList [((x,h), 1) | x <- [0..w-1]]) [h-1,h-2..0] M.! (sx g,0)
  where
    h = length g
    w = length (head g)
    f m y = M.fromList [((x,y), if g!!y!!x == '^' then M.findWithDefault 0 (x-1,y+1) m + M.findWithDefault 0 (x+1,y+1) m else M.findWithDefault 0 (x,y+1) m) | x <- [0..w-1]]

parse :: String -> Item
--parse (d:xs) = (d, read xs)                                              -- (Char,Int)  single-char + number. "L68"
--parse s = let [a,b] = map read (words s) in (a,b)                        -- (Int,Int)   two numbers separated by space. "12 34"
--parse s = map read $ words $ map (\c -> if c==',' then ' ' else c) s     -- [Int]       comma-separated numbers. "1,2,3"
--parse s = read s :: Int                                                  -- Int         single integer per line. "42"
parse s = s                                                              -- String      string per line. "##.#."
--parse s = head s                                                         -- Char        single character per line. "L"

main :: IO ()
main = do
  c <- readFile "7.in"
  let items = map parse (lines c)
  putStrLn $ "Part 1: " ++ show (p1 items)
  putStrLn $ "Part 2: " ++ show (p2 items)
