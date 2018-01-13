import System.Random

type ChromePair = (Char, Char)
type Genome = [ChromePair]

nextGen :: (RandomGen a) => a -> a
nextGen g = snd (next g)

formChrome :: ChromePair -> ChromePair -> Integer -> Integer -> ChromePair
formChrome p1 p2 r1 r2 = (chooseChrome p1 r1, chooseChrome p2 r2)
  where chooseChrome (c1, c2) r = if r == 0 then c1 else c2 

formGenome :: Genome -> Genome -> StdGen -> Genome
formGenome [] [] _  = []
formGenome (p1:p1s) (p2:p2s) g =
  formChrome p1 p2 r1 r2 : formGenome p1s p2s (snd g'')
  where g' = randomR (0,1) g
        g'' = randomR (0,1) (snd g')
        r1 = fst g'
        r2 = fst g''

formGenomes :: [(Genome , Genome)] -> StdGen -> [Genome]
formGenomes [] _ = []
formGenomes ((a,b):xs) g = formGenome a b g : formGenomes xs (nextGen $ nextGen g)

countMatches :: (ChromePair , ChromePair) -> Int
countMatches ((a,b) , (c,d))
  | a == c && b == d = 2
  | a == c = 1
  | b == d = 1
  | otherwise = 0

compareGenomes :: (Genome , Genome) -> Int
compareGenomes (g1, g2) =
  sum [countMatches x | x <- zip g1 g2 ]

parentA = replicate 26 ('a','b')
parentB = replicate 26 ('c','d')

samples = replicate 10000 (parentA, parentB)
  
main = do
  g <- getStdGen
  let childs = formGenomes samples g
  let childs1 = take 5000 childs
  let childs2 = drop 5000 childs
  let c = [ compareGenomes x | x <- zip childs1 childs2]
  let s = fromIntegral (sum c) / 5000
  print s
