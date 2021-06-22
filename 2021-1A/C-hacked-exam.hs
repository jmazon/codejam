{-# LANGUAGE RecordWildCards #-}
import Control.Applicative
import Control.Monad
import Data.List
import Data.Ratio

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,q] <- map read . words <$> getLine
    ss <- replicateM n (readAttempt <$> getLine)
    let sol | n == 2 && q <= 100 = medium1 ss
            | n <= 2 && q <= 10 = small ss
    putStrLn $ "Case #" ++ show i ++ ": " ++ format sol

readAttempt :: String -> Attempt Score
readAttempt l = Attempt (read s) (map (== 'T') e) where [e,s] = words l

format :: Attempt Exp -> String
format Attempt{..} = map (head . show) attemptAnswers ++ " " ++
  show (numerator attemptScore) ++ "/" ++ show (denominator attemptScore)

type Exam = [Bool]
data Attempt a = Attempt
  { attemptScore :: !a
  , attemptAnswers :: [Bool]
  } 
type Score = Integer
type Prob = Rational
type Exp = Rational

instance Num a => Semigroup (Attempt a) where
  Attempt s1 a1 <> Attempt s2 a2 = Attempt (s1+s2) (a1<>a2)
instance Num a => Monoid (Attempt a) where
  mempty = Attempt 0 []

evaluate :: Exam -> Exam -> Score
evaluate = ((genericLength . filter (uncurry (==))) .) .  zip

small :: [Attempt Score] -> Attempt Exp
small as = foldMap computeAnswer (transpose prior)
  where
    universe :: [Exam]
    universe = mapM (const [False,True]) (attemptAnswers (head as))

    isFeasible :: Exam -> Attempt Score -> Bool
    isFeasible truth attempt =
      evaluate truth (attemptAnswers attempt) == attemptScore attempt

    prior :: [Exam]
    prior = filter (\truth -> all (isFeasible truth) as) universe

    computeAnswer :: [Bool] -> Attempt Exp
    computeAnswer bs = if 2*t >= l then Attempt (t/l) [True] else Attempt (1-t/l) [False]
      where t = genericLength (filter id bs)
            l = genericLength bs

data Med = Med { medConst :: !Integer, medTT :: !Integer, medTF :: !Integer }
instance Semigroup Med where Med a b c <> Med d e f = Med (a+d) (b+e) (c+f)
instance Monoid Med where mempty = Med 0 0 0

medium1 :: [Attempt Score] -> Attempt Exp
medium1 [a1,a2] = answer
  where
    pTT = if delta == 0 then 0.5 else fromIntegral ((attemptScore a1-k1)*stf2 - (attemptScore a2-k2)*stf1) / delta
    pTF = if delta == 0 then 0.5 else fromIntegral (stt1*(attemptScore a2-k2) - stt2*(attemptScore a1-k1)) / delta
    delta = fromIntegral $ stt1*stf2 - stt2*stf1
    (Med k1 stt1 stf1,Med k2 stt2 stf2,answer) = mconcat $ zipWith combine (attemptAnswers a1) (attemptAnswers a2)
    combine True True = (Med 0 1 0,Med 0 1 0,if pTT >= 0.5 then Attempt pTT [True] else Attempt (1 - pTT) [False])
    combine True False = (Med 0 0 1,Med 1 0 (-1),if pTF >= 0.5 then Attempt pTF [True] else Attempt (1 - pTF) [False])
    combine False True = (Med 1 0 (-1),Med 0 0 1,if pTF < 0.5 then Attempt (1 - pTF) [False] else Attempt pTF [True])
    combine False False = (Med 1 (-1) 0,Med 1 (-1) 0,if pTT <= 0.5 then Attempt (1 - pTT) [False] else Attempt pTT [True])

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p x = x <$ guard (p x)
