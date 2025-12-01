import System.IO

-- define output type of parse
type Item = 

p1 :: [Item] -> Int
p1 _ = 0

p2 :: [Item] -> Int
p2 _ = 0

parse :: String -> Item
--parse (d:xs) = (d, read xs)                                              -- (Char,Int)  single-char + number. "L68"
--parse s = let [a,b] = map read (words s) in (a,b)                        -- (Int,Int)   two numbers separated by space. "12 34"
--parse s = map read $ words $ map (\c -> if c==',' then ' ' else c) s     -- [Int]       comma-separated numbers. "1,2,3"
--parse s = read s :: Int                                                  -- Int         single integer per line. "42"
--parse s = s                                                              -- String      string per line. "##.#."
--parse s = head s                                                         -- Char        single character per line. "L"

main :: IO ()
main = do
  c <- readFile "1.in"
  let items = map parse (lines c)
  putStrLn $ "Part 1: " ++ show (p1 items)
  --putStrLn $ "Part 2: " ++ show (p2 items)
