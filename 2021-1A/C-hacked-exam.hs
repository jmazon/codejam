{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Main(main,testSet,test3) where

import Control.Monad  (forM_,replicateM,guard)
import Data.List      (genericLength,transpose)
import Data.Ratio
import Data.Semigroup (Sum(Sum))
import Data.Vector    (Vector,(!))
import qualified Data.Vector as V

readAttempt :: String -> Attempt Score
readAttempt l = Attempt (read s) (map (== 'T') e) where [e,s] = words l

format :: Attempt Exp -> String
format Attempt{..} = map (head . show) attemptAnswers ++ " " ++
  show (numerator attemptScore) ++ "/" ++ show (denominator attemptScore)

type Exam = [Bool]
type Score = Int
type Exp = Rational

data Attempt a = Attempt
  { attemptScore :: !a
  , attemptAnswers :: [Bool]
  }
  deriving Show
instance Num a => Semigroup (Attempt a) where
  Attempt s1 a1 <> Attempt s2 a2 = Attempt (s1+s2) (a1++a2)
instance Num a => Monoid (Attempt a) where
  mempty = Attempt 0 []

small :: [Attempt Score] -> Attempt Exp
small as = foldMap computeAnswer (transpose feasibleExams)
  where
    universe :: [Exam]
    universe = mapM (const [False,True]) (attemptAnswers (head as))

    feasibleExams :: [Exam]
    feasibleExams = filter (\truth -> all (isFeasible truth) as) universe

    isFeasible :: Exam -> Attempt Score -> Bool
    isFeasible truth attempt =
      evaluate truth (attemptAnswers attempt) == attemptScore attempt

    evaluate :: Exam -> Exam -> Score
    evaluate = ((genericLength . filter (uncurry (==))) .) . zip

    computeAnswer :: [Bool] -> Attempt Exp
    computeAnswer bs | 2*t >= l   = Attempt (  t/l) [True]
                     | otherwise = Attempt (1-t/l) [False]
      where t = genericLength (filter id bs)
            l = genericLength bs

data Medium = M !Int {-^ const -} !Int {-^ TT -} !Int {-^ TF -}
instance Semigroup Medium where M a b c <> M d e f = M (a+d) (b+e) (c+f)
instance Monoid Medium where mempty = M 0 0 0

medium :: [Attempt Score] -> Attempt Exp
medium [x] = medium [x,x]
medium [Attempt s1 as1,Attempt s2 as2] = answer
  where
    (M k1 stt1 stf1,M k2 stt2 stf2,answer) = mconcat $ zipWith combine as1 as2
    combine True  True  = (M 0 1 0,M 0 1   0, if pTT >= 0.5 then Attempt pTT [True] else Attempt (1 - pTT) [False])
    combine True  False = (M 0 0 1,M 1 0 (-1),if pTF >= 0.5 then Attempt pTF [True] else Attempt (1 - pTF) [False])
    combine False True  = (M 0 0 1,M 1 0 (-1),if pTF < 0.5 then Attempt (1 - pTF) [True] else Attempt pTF [False])
    combine False False = (M 0 1 0,M 0 1   0, if pTT < 0.5 then Attempt (1 - pTT) [True] else Attempt pTT [False])
    (pTT,pTF) = cramer2 (fromIntegral stt1) (fromIntegral stf1) (fromIntegral (s1-k1))
                        (fromIntegral stt2) (fromIntegral stf2) (fromIntegral (s2-k2))
medium xs = error $ "Illegal N for medium: " ++ show (length xs)

cramer1 :: (Eq a,Fractional a) => a -> a -> a
cramer1 0 0 = 0
cramer1 0 _ = error "cramer1: no solution"
cramer1 a b = b/a

-- | solve { a1x+b1y = c1; a2x+b2y = c2 } for (x,y)
cramer2 :: (Show a,Eq a,Fractional a) => a -> a -> a -> a -> a -> a -> (a,a)
cramer2 a1 b1 c1 a2 b2 c2
  | delta /= 0 = ((c1*b2 - b1*c2) / delta,(a1*c2 - c1*a2) / delta)
  | b1 == 0 = (cramer1 a1 c1,0)
  | a1 == 0 = (0,cramer1 b1 c1)
  | otherwise = (0,0) -- arbitrarily (we know there is necessarily a solution from external information)
  where delta = a1*b2 - a2*b1

large :: [Attempt Score] -> Attempt Exp
large [x] = large [x,x,x]
large [x,y] = large [x,x,y]
large [Attempt s1 as1,Attempt s2 as2,Attempt s3 as3] =
  let
    tally :: Vector Int
    tally = V.accum (+) (V.replicate 4 0)
            [ (2*fromEnum (a2 /= a1) + fromEnum (a3 /= a1),1)
            | a1 <- as1 | a2 <- as2 | a3 <- as3 ]

    nsFrom0 :: Int -> Maybe (Vector Int)
    nsFrom0 n0 = do
      let twoN1 = s1 + s2 - 2*n0 - tally!2 - tally!3
      guard (even twoN1)
      guard (twoN1 >= 0)
      guard (twoN1 <= 2 * tally!1)
      let twoN2 = s1 + s3 - 2*n0 - tally!1 - tally!3
      guard (even twoN2)
      guard (twoN2 >= 0)
      guard (twoN2 <= 2 * tally!2)
      let twoN3 = s1 - s2 + tally!2 + tally!3 - twoN2
      guard (even twoN3)
      guard (twoN3 >= 0)
      guard (twoN3 <= 2 * tally!3)
      pure $ V.fromList [n0, twoN1 `div` 2, twoN2 `div` 2, twoN3 `div` 2]

    construct :: Vector (Vector Int) -> Attempt Exp
    construct kss =
      let
        -- | number of exams with a given structure (count of Ts per
        -- question type)
        structureCounts :: Vector Integer
        structureCounts = V.product . V.zipWith ncr tally <$> kss

        -- | structure count -> k -> (score of T,score of F) per question type
        weightedAwards :: Integer -> Vector Int -> Vector (Sum Integer,Sum Integer)
        weightedAwards c = V.zipWith (\n k -> (Sum (c * fromIntegral (n-k))
                                             ,Sum (c * fromIntegral k))) tally

        -- | (score of T,score of F) per exam structure
        typeAwards :: Vector (Sum Integer,Sum Integer)
        typeAwards = V.foldl1' (V.zipWith (<>)) $
                     V.zipWith weightedAwards structureCounts kss

        -- | sum of scores over feasible exams
        typeScores = sum $ uncurry max <$> typeAwards

        -- | expected score
        score = sum typeScores % sum structureCounts

        -- | best choice per answer
        typeAnswers = uncurry (<) <$> typeAwards

        -- | fetch an answer per type and unfold symmetry
        genAnswer a1 a2 a3 =
          typeAnswers ! (2*fromEnum (a2 /= a1)+fromEnum (a3 /= a1)) == a1

        -- | the attempt's bitstring
        answers = zipWith3 genAnswer as1 as2 as3

      in Attempt score answers
  in construct $ V.mapMaybe nsFrom0 $ V.enumFromTo 0 (tally!0)
large xs = error $ "Illegal N for large: " ++ show (length xs)

ncr :: Int -> Int -> Integer
ncr n r = c ! n ! r where
  c = V.iterateN 121 next (V.singleton 1)
  next v = V.zipWith (+) (V.cons 0 v) (V.snoc v 0)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,q] <- map read . words <$> getLine
    ss <- replicateM n (readAttempt <$> getLine)
    let sol | n <= 2 && q <=  10 = small ss
            | n <= 2 && q <=  40 = medium ss
            | n <= 3 && q <= 120 = large ss
            | otherwise = error $ "Unexpected (N,Q): " ++ show (n,q)
    putStrLn $ "Case #" ++ show i ++ ": " ++ format sol

testSet :: [[Attempt Score]]
testSet =
  [ [ Attempt 3 [False,False,True] ]
  , [ Attempt 2 [False,False,True] ]
  , [ Attempt 2 [False,False,True,True,True,False ]
    , Attempt 4 [False,True,False,True,False,True ] ]
  , [ Attempt 1 [False,False]
    , Attempt 1 [True,True] ]
  ]

test3 :: [Attempt Score]
test3 =
  [ Attempt 55 [False,False,True,False,False,False,True,False,False,False,True,True,True,True,True,True,True,False,True,False,False,False,False,False,False,True,True,True,False,True,False,False,False,True,False,True,False,False,True,True,False,True,False,False,True,False,False,True,True,True,False,True,False,True,False,False,True,False,True,False,True,True,False,False,False,False,True,False,True,False,False,False,False,True,True,True,False,True,True,False,True,True,True,True,False,False,False,True,True,False,False,False,False,False,True,True,False,False,True,False,False,True,False,False,True,True,True,False,False,False,False,True,True,False,False,True,False,True,True,False]
  , Attempt 62 [False,False,False,True,False,False,True,True,False,False,False,False,True,False,True,False,False,True,False,False,False,True,True,True,True,True,True,False,False,False,True,True,True,False,True,True,True,True,False,False,True,False,True,True,True,False,True,True,False,False,True,True,True,False,True,False,False,False,False,True,False,False,True,True,False,False,True,True,False,True,True,False,False,True,False,True,False,False,True,False,True,True,False,True,False,True,False,False,True,True,True,False,False,True,False,True,False,True,True,False,False,True,False,True,False,True,False,True,True,False,False,True,False,False,False,True,False,True,False,True]
  , Attempt 64 [False,False,False,True,False,True,True,False,False,False,False,False,True,False,True,False,True,True,True,True,True,True,False,False,True,True,False,True,False,False,False,True,False,False,True,True,True,True,True,True,False,False,False,True,True,True,False,False,False,True,True,False,True,False,False,False,False,False,False,True,False,True,True,False,False,True,False,True,True,True,False,True,True,True,True,False,True,True,False,False,False,False,True,False,False,True,True,False,True,False,False,True,True,True,True,True,True,False,True,False,False,False,False,False,True,True,False,False,True,False,True,False,True,False,False,True,True,True,True,True]
  ]
