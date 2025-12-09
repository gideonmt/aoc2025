import System.IO
import Data.List (tails)

-- define output type of parse
type Item = (Int, Int)

ar :: Item -> Item -> Int
ar (x1,y1) (x2,y2) = (abs(x2-x1)+1) * (abs(y2-y1)+1)

p1 :: [Item] -> Int
p1 ps = maximum [ar p q | (p:qs) <- tails ps, q <- qs]

out :: (Int,Int) -> (Int,Int) -> (Item,Item) -> Bool
out (mn,mx) (my,yy) ((fx,fy),(sx,sy)) = 
  fx<=mn && sx<=mn || fx>=mx && sx>=mx || fy<=my && sy<=my || fy>=yy && sy>=yy

cE :: [Item] -> (Item,Item) -> Bool
cE ps ((x1,y1),(x2,y2)) = all (out (mn,mx) (my,yy)) es
  where
    es = zip ps (tail ps ++ [head ps])
    (mn,mx) = (min x1 x2, max x1 x2)
    (my,yy) = (min y1 y2, max y1 y2)

p2 :: [Item] -> Int  
p2 ps = maximum $ 0 : [ar p q | (p:qs) <- tails ps, q <- qs, cE ps (p,q)]

parse :: String -> Item
--parse (d:xs) = (d, read xs)                                              -- (Char,Int)  single-char + number. "L68"
parse s = let [a,b] = map read (words $ map (\c -> if c==',' then ' ' else c) s) in (a,b) -- (Int,Int)
--parse s = read s :: Int                                                  -- Int         single integer per line. "42"
--parse s = s                                                              -- String      string per line. "##.#."
--parse s = head s                                                         -- Char        single character per line. "L"

main :: IO ()
main = do
  c <- readFile "9.in"
  let items = map parse (lines c)
  putStrLn $ "Part 1: " ++ show (p1 items)
  putStrLn $ "Part 2: " ++ show (p2 items)
