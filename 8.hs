import System.IO
import Data.List (sortBy)
import qualified Data.IntMap as M

-- define output type of parse
type Item = (Int,Int,Int)

dist :: Item -> Item -> Int
dist (x1,y1,z1) (x2,y2,z2) = (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

fi :: M.IntMap (Int,Int) -> Int -> Int
fi m i = if p == i then i else fi m p where (p,_) = m M.! i

un :: M.IntMap (Int,Int) -> Int -> Int -> M.IntMap (Int,Int)
un m i j = 
  let ri = fi m i; rj = fi m j
      (_,si) = m M.! ri; (_,sj) = m M.! rj
  in if ri == rj then m else M.insert ri (ri, si+sj) (M.insert rj (ri,sj) m)

bd :: [Item] -> [(Int,Int,Int)] -> (M.IntMap (Int,Int), [(Int,Int)])
bd bs es = go init es []
  where
    n = length bs
    init = M.fromList [(i,(i,1)) | i <- [0..n-1]]
    go m [] acc = (m, reverse acc)
    go m ((d,i,j):rest) acc = 
      if fi m i == fi m j then go m rest acc
      else go (un m i j) rest ((i,j):acc)

p1 :: [Item] -> Int
p1 bs = product (take 3 (sortBy (flip compare) sz))
  where
    n = length bs
    es = sortBy (\(a,_,_) (b,_,_) -> compare a b) [(dist (bs!!i) (bs!!j), i, j) | i <- [0..n-1], j <- [i+1..n-1]]
    (m, _) = bd bs (take 1000 es)
    sz = [s | i <- [0..n-1], let (p,s) = m M.! i, p == i]

p2 :: [Item] -> Int
p2 bs = x1 * x2
  where
    n = length bs
    es = sortBy (\(a,_,_) (b,_,_) -> compare a b) [(dist (bs!!i) (bs!!j), i, j) | i <- [0..n-1], j <- [i+1..n-1]]
    (_, cs) = bd bs es
    (i,j) = last cs
    (x1,_,_) = bs!!i
    (x2,_,_) = bs!!j

parse :: String -> Item
--parse (d:xs) = (d, read xs)                                              -- (Char,Int)  single-char + number. "L68"
--parse s = let [a,b] = map read (words s) in (a,b)                        -- (Int,Int)   two numbers separated by space. "12 34"
parse s = let [a,b,c] = map read (words (map (\ch -> if ch==',' then ' ' else ch) s)) in (a,b,c)
--parse s = read s :: Int                                                  -- Int         single integer per line. "42"
--parse s = s                                                              -- String      string per line. "##.#."
--parse s = head s                                                         -- Char        single character per line. "L"

main :: IO ()
main = do
  c <- readFile "8.in"
  let items = map parse (lines c)
  putStrLn $ "Part 1: " ++ show (p1 items)
  putStrLn $ "Part 2: " ++ show (p2 items)
