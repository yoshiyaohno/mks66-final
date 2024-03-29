{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Solids where

import Line
import Screen
import Transform

import Data.Array.Unboxed
import Control.Applicative
import Control.Monad.State
import qualified Data.List  as L
import qualified Data.Set   as S
import qualified Data.Map   as M

import Data.Semigroup as Sem

import Control.Exception

type VertNorms = M.Map (Vect Double) (Vect Double)

newtype Triangle a = Triangle (Vect a, Vect a, Vect a) deriving (Show, Eq)
newtype Pixel = Pixel {getTT :: ((Int, Int), Double)}
    deriving (Eq, Ord, Show)

piStep :: Floating a => a
piStep = pi/11
--piStep = pi/21
--piStep = pi/51

trVNs :: Transform Double -> VertNorms -> VertNorms
trVNs t = M.mapKeys (pmult t) . M.map (pmult t)

avgNorms :: [Vect Double] -> Vect Double
avgNorms vs = normalize . fmap (/(fromIntegral $ length vs)) $
    foldr (liftA2 (+)) (pure 0) vs

vertNorms :: [Triangle Double] -> VertNorms
vertNorms = M.map avgNorms . M.fromListWith (++) . concatMap _tNorms
    where _tNorms t@(Triangle (a,b,c)) =
            let n = normal t in [(a,[n]), (b,[n]), (c,[n])]

torusNormals :: Double -> Double -> Double -> Double -> Double
    -> (VertNorms, [Triangle Double])
torusNormals cx cy cz r0 r1 = 
    let tris = torus cx cy cz r0 r1
        nrms = vertNorms tris
    in  (nrms, tris)

sphereNormals :: Double -> Double -> Double -> Double
    -> (VertNorms, [Triangle Double])
sphereNormals cx cy cz r = 
    let tris = sphere cx cy cz r
        nrms = vertNorms tris
    in  (nrms, tris)

boxNormals :: Double -> Double -> Double -> Double -> Double -> Double
    -> (VertNorms, [Triangle Double])
boxNormals cx cy cz w h d =
    let tris = box cx cy cz w h d
        nrms = vertNorms tris
    in  (nrms, tris)

--  let [p000, p001, p010, p011, p100, p101, p110, p111] = 
--          [Vect (cx + qx * w) (cy + qy * h) (cz + qz * d) 1
--              | qx <- [0,1], qy <- [0,1], qz <- [0,1]]
--  in  (M.map normalize . M.fromList $
--      [(p000, Vect (-1) (-1) (-1) 0), (p001, Vect (-1) (-1) 1 0)
--      ,(p010, Vect (-1) 1 (-1) 0), (p011, Vect (-1) 1 1 0)
--      ,(p100, Vect 1 (-1) (-1) 0), (p101, Vect 1 (-1) 1 0)
--      ,(p110, Vect 1 1 (-1) 0), (p111, Vect 1 1 1 0)]
--      ,
--      stitch4 p000 p010 p110 p100
--      ++ stitch4 p100 p110 p111 p101
--      ++ stitch4 p111 p011 p001 p101
--      ++ stitch4 p110 p010 p011 p111
--      ++ stitch4 p011 p010 p000 p001
--      ++ stitch4 p000 p100 p101 p001)

box :: (Floating a, Enum a) => a -> a -> a -> a -> a -> a -> [Triangle a]
box cx cy cz w h d = let
    [p000, p001, p010, p011, p100, p101, p110, p111] = 
        [Vect (cx + qx * w) (cy + qy * h) (cz + qz * d) 1
            | qx <- [0,1], qy <- [0,1], qz <- [0,1]]
                in stitch4 p000 p010 p110 p100
                   ++ stitch4 p100 p110 p111 p101
                   ++ stitch4 p111 p011 p001 p101
                   ++ stitch4 p110 p010 p011 p111
                   ++ stitch4 p011 p010 p000 p001
                   ++ stitch4 p000 p100 p101 p001
    -- y a h o o

vecterpolateY :: (Vect Double, Vect Double) -> (Vect Double, Vect Double)
    -> [((Int, Int), Vect Double)]
vecterpolateY (v0, n0@(Vect _ ny0 _ _)) (v1, n1@(Vect _ ny1 _ _)) =
    [((round x, round y), nlorpy y) | (Vect x y _ _) <-
            lh getY (fromIntegral.round<$>v0) (fromIntegral.round<$>v1) ]
        where nlorpy y = lerp n0 n1 $ (y - getY v0) / (getY v1 - getY v0)

vecterpolateX :: ((Int, Int), Vect Double) -> ((Int, Int), Vect Double)
    -> [((Int, Int), Vect Double)]
vecterpolateX ((x0,y0), n0@(Vect nx0 _ _ _)) ((x1,y1), n1@(Vect nx1 _ _ _))
    | y0 /= y1  = error $ "mismatched y values in vecterpolateX:\ny0: "
                        <> show y0 <> "\ny1: " <> show y1
    | x0 == x1  = [((x0,y0),n0)]
    | otherwise = --trace ("y0: " <> show y0 <> "\ny1: " <> show y1 <> "\n") $
        let nlorpx x = lerp n0 n1 $ x / fromIntegral (x1 - x0)
        in  [((x+x0, y0), nlorpx . fromIntegral $ x)
                | x <- [0, signum (x1-x0) .. (x1-x0)]]

scanTriangle :: Triangle Double -> [Pixel]
scanTriangle (Triangle (a, b, c)) = let
    [bot, mid, top] = map vdToPix (L.sortOn getY [a, b, c])
    e1 = pixLiner bot top
    e2 = pixLiner mid top ++ tail (pixLiner bot mid)
    es = if 2*(pgetX mid) <= (pgetX top + pgetX bot)
            then zip e2 e1 else zip e1 e2
    in concatMap (uncurry pixScan) es

lh :: (Eq a, Enum a, Fractional a) =>
    (Vect a -> a) -> Vect a -> Vect a -> [Vect a]
lh = lineHelper

-- returns the vertical line between two pixels
pixLiner :: Pixel -> Pixel -> [Pixel]
pixLiner p0 p1 = map vdToPix (lh getY (pixToVd p0) (pixToVd p1))
    -- ......
    -- I give up.

-- scans across a line of pixels (finding z values in between)
pixScan :: Pixel -> Pixel -> [Pixel]
pixScan (Pixel ((x0,y0),z0)) (Pixel ((x1,y1),z1))
    | y0 /= y1  = error "mismatched y values in pixScan"
    | dx == 0   = [Pixel ((x0,y0),z0)]
    | otherwise = [Pixel ((x+x0, y0),
                    (fromIntegral x)*dz/(fromIntegral dx) + z0)
                    | x <- [0, (signum dx) .. dx]]
    where dx = x1 - x0; dz = z1 - z0

{-# INLINE pgetX #-}
pgetX :: Pixel -> Int
pgetX = fst.fst.getTT

{-# INLINE pgetY #-}
pgetY :: Pixel -> Int
pgetY = snd.fst.getTT

vdToPix :: Vect Double -> Pixel
vdToPix (Vect x y z q) = Pixel ((round x, round y), z)
-- why
pixToVd :: Pixel -> Vect Double
pixToVd (Pixel ((x,y),z)) = Vect (fromIntegral x) (fromIntegral y) z 1

toEdges :: Triangle a -> [Line a]
toEdges (Triangle (a, b, c)) = [Line a b, Line b c, Line a c]

bfCull :: (Num a, Ord a) => [Triangle a] -> [Triangle a]
bfCull = filter ((>0) . getZ . normal)

normal :: (Num a) => Triangle a -> Vect a
normal (Triangle (a, b, c)) = crossProd ((-) <$> c <*> a) ((-) <$> b <*> a)

trTriangle :: (Num a) => Transform a -> Triangle a -> Triangle a
trTriangle t (Triangle (a, b, c)) = Triangle (pmult t a, pmult t b, pmult t c)


sphere :: (Floating a, Enum a) => a -> a -> a -> a -> [Triangle a]
sphere cx cy cz r = concat $ zipWith stitchLines (rotate 1 arcs) arcs
    where arcs = [[Vect (cx + r * cos thet) (cy + r * sin thet * cos phi)
                 (cz + r * sin thet * sin phi) 1
                 | thet <- [0, piStep .. pi]] | phi <- [0, 2*piStep .. 2*pi]]

torus :: (Floating a, Enum a) => a -> a -> a -> a -> a -> [Triangle a]
torus cx cy cz r0 r1 = concat $ zipWith stitchLines arcs (rotate 1 arcs)
    where arcs = [[Vect (cx + r0 * cos thet * cos phi + r1 * cos phi)
                  (cy + r0 * sin thet)
                  (cz - sin phi * (r0 * cos thet + r1)) 1
                  | thet <- [0, 2*piStep .. 2*pi]]
                  | phi <- [0, piStep .. 2*pi]]

stitchLines :: [Vect a] -> [Vect a] -> [Triangle a]
stitchLines [] _   = []
stitchLines _ []   = []
stitchLines _ [b0] = []
stitchLines [a0] _ = []
stitchLines (a0:a1:as) (b0:b1:bs) =
    (stitch4 a1 a0 b0 b1) ++ (stitchLines (a1:as) (b1:bs))

-- these four points go clockwise to face the normal into you
stitch4 :: Vect t -> Vect t -> Vect t -> Vect t -> [Triangle t]
stitch4 a b c d = [Triangle (a, b, c), Triangle (a, c, d)]

