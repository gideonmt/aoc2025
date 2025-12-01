import System.IO

data Dir = L | R deriving (Read, Show)
data Rot = Rot Dir Int deriving (Read, Show)

ad :: Int -> Rot -> Int
ad p (Rot L n) = mod (p - n) 100
ad p (Rot R n) = mod (p + n) 100

hD :: Int -> Rot -> Int
hD p (Rot L n) =
  let step = -1
      target = mod (-p) 100
      k0 = mod (-target) 100
      first = if k0 == 0 then 100 else k0
  in if first > n then 0 else 1 + (n - first) `div` 100

hD p (Rot R n) =
  let step = 1
      target = mod (-p) 100
      k0 = target
      first = if k0 == 0 then 100 else k0
  in if first > n then 0 else 1 + (n - first) `div` 100

p1 :: [Rot] -> Int
p1 = go 50
  where
    go _ [] = 0
    go p (r:rs) =
      let q = ad(p) r
      in (if q == 0 then 1 else 0) + go q rs

p2 :: [Rot] -> Int
p2 = go 50
  where
    go _ [] = 0
    go p (r:rs) =
      let q = ad p r
      in hD p r + go q rs

parse :: String -> Rot
parse (d:xs) = Rot (read [d]) (read xs)

main :: IO ()
main = do
  c <- readFile "1.in"
  let rs = map parse (lines c)
  putStrLn $ "Part 1: " ++ show (p1 rs)
  putStrLn $ "Part 2: " ++ show (p2 rs)
