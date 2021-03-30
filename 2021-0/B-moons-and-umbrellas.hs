{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

import Data.List     (foldl')
import Control.Monad (forM_)
import Control.Arrow ((&&&))

default (Int)

fixASCII ∷ String → String
fixASCII = map $ \case 'C' → '🌘'
                       'J' → '🌂'
                       '?' → '❓'
                       x   →  x

main ∷ IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i → do
    [x,y,s] <- words . fixASCII <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (invoice (read x) (read y) s)

data (│) = (:🌘) Int | (:🌂) Int | (:🌘🌂) Int Int

minimal ∷ (│) → Int
minimal = merge min

transition ∷ Int → Int → Char → (│) → (│)
transition x y = \case '🌘' →         (:🌘)   .  (<|🌘)
                       '🌂' →         (:🌂)   .             (<|🌂)
                       '❓' → uncurry (:🌘🌂) . ((<|🌘) &&& (<|🌂))
  where (<|🌘) = minimal . (🌂) (+y)
        (<|🌂) = minimal . (🌘) (+x)

start ∷ Char → (│)
start '🌘' = (:🌘) 0
start '🌂' = (:🌂) 0
start '❓' = (:🌘🌂) 0 0

invoice ∷ Int → Int → String → Int
invoice x y (h:t) = minimal $ foldl' (flip $ transition x y) (start h) t

-- `first` and `second` from `Bifunctor`
(🌘),(🌂) ∷ (Int → Int) → (│) → (│)
(🌘) f = bimap f id
(🌂) f = bimap id f

-- `bimap` from `Bifunctor`
bimap ∷ (Int → Int) → (Int → Int) → (│) → (│)
bimap f _ ((:🌘) a) = (:🌘) (f a)
bimap _ g ((:🌂) b) = (:🌂) (g b)
bimap f g ((:🌘🌂) a b) = (:🌘🌂) (f a) (g b)

-- `mergeThese` from `these`
merge ∷ (Int → Int → Int) → (│) → Int
merge _  ((:🌘) a) = a
merge _  ((:🌂) b) = b
merge op ((:🌘🌂) a b) = op a b
