import System.IO

-- define output type of parse
type Item = (Int, Int)

p1 :: [Item] -> Int
p1 rs = sum [n | (s, e) <- rs, n <- [s..e], c1 n]

p2 :: [Item] -> Int
p2 rs = sum [n | (s, e) <- rs, n <- [s..e], c2 n]

c1 :: Int -> Bool
c1 n = 
  let s = show n
      l = length s
  in even l && l > 0 &&
     let (a, b) = splitAt (l `div` 2) s
     in a == b && head a /= '0'

c2 :: Int -> Bool
c2 n = 
  let s = show n
  in head s /= '0' && cyc s

cyc :: String -> Bool
cyc s = 
  let l = length s
      d = s ++ s
  in any (\c -> c < l && l `mod` c == 0 && take l (drop c d) == s) [1..l `div` 2]

parse :: String -> Item
--parse (d:xs) = (d, read xs)                                              -- (Char,Int)  single-char + number. "L68"
--parse s = let [a,b] = map read (words s) in (a,b)                        -- (Int,Int)   two numbers separated by space. "12 34"
--parse s = map read $ words $ map (\c -> if c==',' then ' ' else c) s     -- [Int]       comma-separated numbers. "1,2,3"
--parse s = read s :: Int                                                  -- Int         single integer per line. "42"
--parse s = s                                                              -- String      string per line. "##.#."
--parse s = head s                                                         -- Char        single character per line. "L"
parse s = let [a, b] = map read $ words $ map (\c -> if c == '-' then ' ' else c) s in (a, b)

main :: IO ()
main = do
  c <- readFile "2.in"
  let allRanges = words $ map (\c -> if c == ',' then ' ' else c) c
      items = map parse allRanges
  putStrLn $ "Part 1: " ++ show (p1 items)
  putStrLn $ "Part 2: " ++ show (p2 items)
