module Line where

import Screen
import Data.Array.Unboxed --hmm
import Control.Applicative
import qualified Data.List as L

data Line a = Line (Vect a) (Vect a) deriving (Show)
data Vect a = Vect { getX::a
                   , getY::a
                   , getZ::a
                   , getQ::a
                   } deriving (Eq, Ord)

instance (Show t) => Show (Vect t) where
    show (Vect x y z q) = "("
                            ++ show x ++ ", "
                            ++ show y ++ ", "
                            ++ show z ++ ", "
                            ++ show q
                            ++ ")"

instance Functor Line where
    fmap f (Line p0 p1) = Line (fmap f p0) (fmap f p1)

instance Functor Vect where
    fmap f (Vect x y z q) = Vect (f x) (f y) (f z) (f q)
    
instance Applicative Vect where
    pure x = (Vect x x x x)
    (Vect f0 f1 f2 f3) <*> (Vect x y z q) =
        (Vect (f0 x) (f1 y) (f2 z) (f3 q))

instance Foldable Vect where
    foldr f acc (Vect x0 x1 x2 x3) =
        foldr f acc [x0, x1, x2, x3]

normalize :: (Floating a) => Vect a -> Vect a
normalize v = fmap (/(sqrt $ dot v v)) v

dot :: (Num a) => Vect a -> Vect a -> a
dot p = sum . liftA2 (*) p

lineHelper :: (Eq a, Enum a, Fractional a) => 
    (Vect a -> a) -> Vect a -> Vect a -> [Vect a]
lineHelper f v0 v1
    | f v0 == f v1  = [v0]
    | otherwise     = map (lerp v0 v1) [0, (1/(f v1 - f v0)) .. 1]

lerp :: (Num a) => Vect a -> Vect a -> a -> Vect a
lerp v0 v1 = (((liftA2 (\a b t -> a*t + b*(1 - t))) v0 v1) <*>).pure
--lerp v0 v1 t = (+) <$> ((*) <$> v0 <*> pure t) 

crossProd :: (Num a) => Vect a -> Vect a -> Vect a
crossProd (Vect x0 y0 z0 _) (Vect x1 y1 z1 _)
    = (Vect (y0*z1 - z0*y1) (x0*z1 - x1*z0) (x0*y1 - y0*x1) 1)

drawLine :: Color -> Line Int -> Screen -> Screen
drawLine c ln = draw [((getX px, getY px), c) | px <- rasterLine ln]

drawEdges :: (RealFrac a) => Color -> [Vect a] -> Screen -> Screen
drawEdges c =
    foldr (.) id . map (drawLine c . uncurry Line) . pairOff . (map.fmap) round 

addLine :: Line a -> [Vect a] -> [Vect a]
addLine (Line p0 p1) = ([p0, p1] ++)

connectPts :: [Vect a] -> [Vect a]
connectPts [] = []
connectPts [x] = []
connectPts (a:b:xs) = a:b:(connectPts $ b:xs)

toList :: Vect a -> [a]
toList = foldr (:) []

-- all (non-permuted) pairs of a list
allPairs :: [a] -> [(a, a)]
allPairs []     = []
allPairs (_:[]) = []
allPairs (x:xs) = map ((,) x) xs ++ allPairs xs
-- DIFFERENT: pair off elements of a list
pairOff :: [a] -> [(a, a)]
pairOff []       = []
pairOff (x:[])   = []
pairOff (a:b:xs) = ((a,b) : pairOff xs)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x,y) = (x, f y)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n $ cycle xs) xs

-- just gives you the points a line covers, no color
rasterLine :: (Integral a) => Line a -> [Vect a]
rasterLine (Line p0 p1)
    | dy == 0 && dx == 0        = [p0]
    | abs dx > abs dy && dx > 0 = _rLx (Line p0 p1)
    | abs dx > abs dy           = _rLx (Line p1 p0)
    | dy > 0                    = _rLy (Line p0 p1)
    | otherwise                 = _rLy (Line p1 p0)
    where   dy = (getY p1) - (getY p0) 
            dx = (getX p1) - (getX p0)

-- hell yeah ugly helper functions
--  (I could use only one by flipping the tuples somehow but ergh)
_rLx :: (Integral a) => Line a -> [Vect a]
_rLx (Line (Vect x0 y0 _ _) (Vect x1 y1 _ _)) =
    L.zipWith4 (Vect) [x0..x1] ys (repeat 0) (repeat 1)
    where   ys = map ((+y0) . (`quot` (2* abs dx))) . tail $ [negate dy, dy..]
            dy = y1 - y0
            dx = x1 - x0

_rLy :: (Integral a) => Line a -> [Vect a]
_rLy (Line (Vect x0 y0 _ _) (Vect x1 y1 _ _)) =
    L.zipWith4 (Vect) xs [y0..y1] (repeat 0) (repeat 1)
    where   xs = map ((+x0) . (`quot` (2* abs dy))) . tail $ [negate dx, dx..]
            dy = y1 - y0
            dx = x1 - x0

