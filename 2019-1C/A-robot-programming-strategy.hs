import Data.Maybe
import Data.List
import Control.Monad

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    a <- readLn
    cs <- map cycle <$> replicateM a getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ maybe "IMPOSSIBLE" id (go cs 0)

go cs l | null cs || null (head cs) = return []
        | l == 500 = Nothing
        | otherwise = case c' of
            "PRS" -> Nothing
            "PR"  -> next 'P' 'R'
            "RS"  -> next 'R' 'S'
            "PS"  -> next 'S' 'P'
            "P"   -> next 'S' 'P'
            "R"   -> next 'P' 'R'
            "S"   -> next 'R' 'S'
  where c' = map head $ group $ sort $ map head cs
        next pick elim = fmap (pick :) $
                         go (mapMaybe (d elim) cs) (l+1)
        d elim (h:t) = guard (h /= elim) *> pure t
