import System.IO
import Data.Array

-- define output type of parse
type Item = String

-- count @ neighbors w/ 8 directions
nb :: Array (Int,Int) Char -> Int -> Int -> (Int,Int) -> Int
nb g h w (r,c) = length [(dr,dc) | dr <- [-1..1], dc <- [-1..1], 
                                   (dr,dc) /= (0,0),
                                   let r' = r+dr, let c' = c+dc,
                                   r' >= 0, r' < h, c' >= 0, c' < w,
                                   g!(r',c') == '@']

-- make grid
mkG :: [Item] -> (Array (Int,Int) Char, Int, Int, [(Int,Int)])
mkG ls = (g, h, w, ps)
  where
    h = length ls
    w = length (head ls)
    g = listArray ((0,0),(h-1,w-1)) (concat ls)
    ps = [(r,c) | r <- [0..h-1], c <- [0..w-1], g!(r,c) == '@']

-- count @ with <4 @ neighbors
p1 :: [Item] -> Int
p1 ls = length [() | p <- ps, nb g h w p < 4]
  where (g,h,w,ps) = mkG ls

-- simulate removal
-- queue starts with accessible @, remove them, check if neighbors become accessible
p2 :: [Item] -> Int
p2 ls = go q0 g0
  where
    (g0,h,w,ps) = mkG ls
    q0 = [p | p <- ps, nb g0 h w p < 4]  -- initial queue all accessible @
    go [] g = length [() | i <- indices g, g!i == 'x']  -- count removed
    go (p:qs) g
      | g!p /= '@' = go qs g  -- already removed, skip
      | otherwise = go (qs ++ new) g'  -- remove @ and add newly accessible neighbors to queue
      where
        g' = g // [(p,'x')]  -- mark as removed
        adj = [(r+dr,c+dc) | let (r,c) = p, dr <- [-1..1], dc <- [-1..1],
                             (dr,dc) /= (0,0),
                             let r' = r+dr, let c' = c+dc,
                             r' >= 0, r' < h, c' >= 0, c' < w]
        new = [a | a <- adj, g'!a == '@', nb g' h w a < 4]  -- neighbors now accessible

parse :: String -> Item
--parse (d:xs) = (d, read xs)                                              -- (Char,Int)  single-char + number. "L68"
--parse s = let [a,b] = map read (words s) in (a,b)                        -- (Int,Int)   two numbers separated by space. "12 34"
--parse s = map read $ words $ map (\c -> if c==',' then ' ' else c) s     -- [Int]       comma-separated numbers. "1,2,3"
--parse s = read s :: Int                                                  -- Int         single integer per line. "42"
parse s = s                                                              -- String      string per line. "##.#."
--parse s = head s                                                         -- Char        single character per line. "L"

main :: IO ()
main = do
  c <- readFile "4.in"
  let items = map parse (lines c)
  putStrLn $ "Part 1: " ++ show (p1 items)
  putStrLn $ "Part 2: " ++ show (p2 items)
