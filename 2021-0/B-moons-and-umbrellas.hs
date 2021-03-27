{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Applicative (liftA2,(<|>))

default (Int)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [x,y,s] <- words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (invoice (read x) (read y) s)

data St = St { endC :: Maybe Int, endJ :: Maybe Int } deriving Show

invoice :: Int -> Int -> String -> Int
invoice x y (h:t) = fromMaybe (error "Nonsense!") $ minSt $ foldl' f (st0 h) t where
  st0 'C' = St { endC = Just 0,  endJ = Nothing }
  st0 'J' = St { endC = Nothing, endJ = Just 0  }
  st0 '?' = St { endC = Just 0,  endJ = Just 0  }
  
  f St{..} = \case
    'C' -> St { endC = min' endC ((+ y) <$> endJ), endJ = Nothing }
    'J' -> St { endJ = min' endJ ((+ x) <$> endC), endC = Nothing }
    '?' -> St { endC = min' endC ((+ y) <$> endJ), endJ = min' endJ ((+ x) <$> endC) }

min' :: Ord a => Maybe a -> Maybe a -> Maybe a
min' a b = liftA2 min a b <|> a <|> b

minSt :: St -> Maybe Int
minSt St{..} = min' endC endJ
