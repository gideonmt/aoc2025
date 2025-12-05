import System.IO
import Data.List (sortBy)

-- define output type of parse
type Item = Either (Int,Int) Int

p1 :: [Item] -> Int
p1 xs = length (filter (\i -> any (\(l,h) -> i >= l && i <= h) rs) ids)
  where
    rs = [r | Left r <- xs]
    ids = [i | Right i <- xs]

p2 :: [Item] -> Int
p2 xs = sum (map (\(l,h) -> h - l + 1) (mg (sortBy (\(a,_) (b,_) -> compare a b) rs)))
  where
    rs = [r | Left r <- xs]
    mg [] = []
    mg [r] = [r]
    mg ((l1,h1) : (l2,h2) : rest)
      | h1 >= l2 - 1 = mg ((l1, max h1 h2) : rest)
      | otherwise = (l1,h1) : mg ((l2,h2) : rest)

parse :: String -> Item
--parse (d:xs) = (d, read xs)                                              -- (Char,Int)  single-char + number. "L68"
--parse s = let [a,b] = map read (words s) in (a,b)                        -- (Int,Int)   two numbers separated by space. "12 34"
--parse s = map read $ words $ map (\c -> if c==',' then ' ' else c) s     -- [Int]       comma-separated numbers. "1,2,3"
--parse s = read s :: Int                                                  -- Int         single integer per line. "42"
--parse s = s                                                              -- String      string per line. "##.#."
--parse s = head s                                                         -- Char        single character per line. "L"
parse s | elem '-' s = let [a,b] = map read (words (map (\c -> if c=='-' then ' ' else c) s)) in Left (a,b)
        | otherwise = Right (read s)

main :: IO ()
main = do
  c <- readFile "5.in"
  let ls = lines c
      items = map parse $ filter (not . null) ls
  putStrLn $ "Part 1: " ++ show (p1 items)
  putStrLn $ "Part 2: " ++ show (p2 items)
