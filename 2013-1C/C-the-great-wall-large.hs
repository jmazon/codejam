{-# LANGUAGE MultiParamTypeClasses #-}
import Data.FingerTree
import qualified Data.PriorityQueue.FingerTree as Q
import Data.Monoid (Monoid(mempty,mappend))
import Data.Foldable (foldl',toList)
import Control.Monad (forM_,liftM,replicateM)
import Data.Maybe (catMaybes)
data Attack = Attack { attackDay      :: !Int, attackCount  :: !Int
                     , attackWest     :: !Int, attackEast   :: !Int
                     , attackStrength :: !Int, attackDeltaD :: !Int
                     , attackDeltaP   :: !Int, attackDeltaS :: !Int }
              deriving Show
data Wall = Wall { wallWest :: !Int, wallEast :: !Int, wallStrength :: !Int }
            deriving Show
instance Monoid Wall where mempty = Wall maxBound minBound maxBound
                           mappend (Wall aw ae as) (Wall bw be bs) =
                               Wall (min aw bw) (max ae be) (min as bs)
instance Measured Wall Wall where measure = id
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    as <- liftM (foldl' enqueue Q.empty) $ replicateM n $ do
      [d,c,w,e,s,dd,dp,ds] <- liftM (map read . words) getLine
      return $ Attack d c (2*w) (2*e) s dd (2*dp) ds
    let wall = singleton $ Wall minBound maxBound 0
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve wall as 0)
solve walls attacks c
    | Q.null attacks = c
    | otherwise      = solve walls' attacks' $! c + successes
    where Just (firstAttack,_) = Q.minView attacks
          day = attackDay firstAttack
          (dayAttacks,restAttacks) = getAttacks [] attacks
          getAttacks r attacks
              | Q.null attacks || attackDay a > day = (r,attacks)
              | otherwise                           = getAttacks (a:r) attacks'
              where Just (a,attacks') = Q.minView attacks
          daySuccess a = fst (wLevel walls a)
          successes = length $ filter daySuccess dayAttacks
          walls' = foldl' (\w -> snd . wLevel w) walls dayAttacks
          attacks' = foldl' enqueue restAttacks $
                     catMaybes $ map nextAttack dayAttacks
enqueue q a = Q.insert (attackDay a) a q
nextAttack (Attack d c w e s dd dp ds)
    | c <= 1    = Nothing
    | otherwise = Just (Attack (d+dd) (c-1) (w+dp) (e+dp) (s+ds) dd dp ds)
wLevel w (Attack _ _ aw ae s _ _ _) = ( wallStrength (measure middleWalls) < s,
                                        westWalls >< leveledWalls >< eastWalls )
    where (westWalls,nonWestWalls) = split ((>= aw) . wallEast) w
          (middleWalls',eastWalls') = split ((>= ae) . wallEast) nonWestWalls
          borderEast :< eastWalls = viewl eastWalls'
          middleWalls = middleWalls' |> borderEast
          leveledWalls = fromList $ compress $ inBorder level $ toList middleWalls
          inBorder f w = bww ++ map f mid ++ bee
              where (westWall:w') = w
                    splitWest = wallWest westWall < aw
                    (bww,bwe) | splitWest = ( [ westWall { wallEast = aw-1 } ]
                                            , westWall { wallWest = aw } )
                              | otherwise = ( [], westWall )
                    w'' = bwe : w'
                    w''' = init w''
                    east = last w''
                    splitEast = wallEast east > ae
                    (bew,bee) | splitEast = ( east { wallEast = ae }
                                            , [ east { wallWest = ae+1 } ] )
                              | otherwise = ( east, [] )
                    mid = w''' ++ [bew]
          level w@(Wall ww we ws) = if ws < s then Wall ww we s else w
compress (a@(Wall aw ae as) : ws @ (Wall bw be bs : ws'))
    | as == bs  = compress (Wall aw be as : ws')
    | otherwise = a : compress ws
compress ws = ws
