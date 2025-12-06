import System.IO
import Data.List (transpose, groupBy)
import Data.Char (isDigit, isSpace)

-- define output type of parse
type Item = String

isSp :: String -> Bool
isSp = all isSpace

grp :: [String] -> [[String]]
grp = filter (not . null) . filter (not . isSp . head) . groupBy (\a b -> not (isSp a) && not (isSp b))

sv :: [String] -> [String] -> Int
sv g rws = let ns = [read (filter isDigit r) :: Int | r <- rws, any isDigit r]
               op = last (head g)
           in if op == '*' then product ns else sum ns

p1 :: [Item] -> Int
p1 ls = sum $ map (\g -> sv g (transpose g)) $ grp $ transpose ls

p2 :: [Item] -> Int
p2 ls = sum $ map (\g -> sv g [init c | c <- g]) $ reverse $ grp $ transpose ls

parse :: String -> Item
--parse (d:xs) = (d, read xs)                                              -- (Char,Int)  single-char + number. "L68"
--parse s = let [a,b] = map read (words s) in (a,b)                        -- (Int,Int)   two numbers separated by space. "12 34"
--parse s = map read $ words $ map (\c -> if c==',' then ' ' else c) s     -- [Int]       comma-separated numbers. "1,2,3"
--parse s = read s :: Int                                                  -- Int         single integer per line. "42"
parse s = s                                                              -- String      string per line. "##.#."
--parse s = head s                                                         -- Char        single character per line. "L"

main :: IO ()
main = do
  c <- readFile "6.in"
  let items = map parse (lines c)
  putStrLn $ "Part 1: " ++ show (p1 items)
  putStrLn $ "Part 2: " ++ show (p2 items)
