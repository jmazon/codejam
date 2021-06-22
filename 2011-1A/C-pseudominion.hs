{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Maybe
import Data.List
import qualified Data.Map as M
import Control.Monad.State
import Control.DeepSeq

type Card = (Int,Int,Int)
data Game = Game { gameHand :: [Card]
                 , gameDeck :: [Card]
                 , gameScore :: Int
                 , gameTurns :: Int
                 , gameCards :: Int } deriving Show
instance NFData Game where
    rnf (Game h d s t c) = rnf h `seq`
                           rnf d `seq`
                           rnf s `seq`
                           rnf t `seq`
                           rnf c


cardC :: (c,s,t) -> c
cardC (c,_,_) = c
cardS :: (c,s,t) -> s
cardS (_,s,_) = s
cardT :: (c,s,t) -> t
cardT (_,_,t) = t

game' :: Game -> State (M.Map (Int,Int,Int) Int) Int
game' g = do
  c <- get
  let k = (gameCards g,gameScore g,gameTurns g)
  case M.lookup k c of
    Nothing -> do
      v <- game g
      put (M.insert k v c)
      return v
    Just v -> return v

game :: Game -> State (M.Map (Int,Int,Int) Int) Int
game g | null (gameHand g) || gameTurns g == 0 = return $ gameScore g
       | null ts   = liftM maximum . mapM (game' . play g) $ nexts (gameHand g)
       | otherwise = game' $ foldl' play g ts
    where ts = filter ((>0) . cardT) (gameHand g)

play :: Game -> Card -> Game
play g c = force $
           Game (delete c (gameHand g) ++ d)
                d'
                (gameScore g + cardS c)
                (gameTurns g + cardT c - 1)
                (gameCards g + cardC c)
    where (d,d') = splitAt (cardC c) (gameDeck g)

nexts :: [(Int,Int,Int)] -> [(Int,Int,Int)]
nexts cs = catMaybes $ map f [0..2]
    where f n = case filter ((==n) . cardC) cs of
                  [] -> Nothing
                  cs' -> Just (maximum cs')

main :: IO ()
main = interact $ unlines . zipWith c [1 :: Int ..] . map solve .
                  parse . map read . tail . words
  where c i j = "Case #" ++ show i ++ ": " ++ show j

parse :: [Int] -> [Game]
parse [] = []
parse (n:ns) = Game h d 0 1 n : parse ns'
    where (rh,(m:ms)) = splitAt (3*n) ns
          (rd,ns') = splitAt (3*m) ms
          h = rc rh
          d = rc rd
          rc (a:b:c:ds) = (a,b,c) : rc ds
          rc [] = []

solve :: Game -> Int
solve g = evalState (game' g) M.empty
