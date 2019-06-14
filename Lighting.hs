{-# LANGUAGE FlexibleContexts #-}
module Lighting where

import Line
import Screen
import Solids
import DrawMats

import Data.Array.Unboxed
import Control.Monad.State
import qualified Data.Map   as M
import qualified Data.List  as L

import Debug.Trace --baha

drawTriangles :: (MonadState DrawMats m) => Material -> VertNorms
    -> [Triangle Double] -> m ()
drawTriangles mat vn trs = do
    dm <- get
    let trs' = bfCull trs
        pxs = concatMap scanTriangle trs'
        (toDraw, newZB) = runState (plotPxs pxs) (getZBuf dm)
        c = colorTriangles mat vn trs'
    put $ dm {getZBuf = newZB}
    modify.modScreen $ draw [((x, y), c M.! (x,y)) | Pixel ((x,y),_) <- toDraw]

--drawTriangles :: (MonadState DrawMats m) => Material -> [Triangle Double] -> m ()
--drawTriangles mat = mapM_ (drawTriangle mat) . bfCull

colorNormal :: Material -> Vect Double -> Color
colorNormal (Material kar kdr ksr kag kdg ksg kab kdb ksb ir ig ib alph) nm =
    let norm = normalize nm
        view = Vect 0 0 1 0
        hhhh = normalize $ Vect (-1) 2 3 0     -- halfway vector
        lght = normalize $ Vect (-1) 2 2 0
        hDn  = max 0 $ hhhh`dot`norm        -- reflecton intensity ish
        lDn  = max 0 $ lght`dot`norm        -- diffuse intensity ish
        ia   = 50; id = 255; is = 255
        -- above here is the lighting stuff--that'll have to change some day
        theCalc ka kd ks alp =
            (ka*ia + kd*lDn*id + is*ks*(hDn**alp))      -- crunchy coding
        intense_r = min 255 . round $ theCalc kar kdr ksr alph
        intense_g = min 255 . round $ theCalc kag kdg ksg alph
        intense_b = min 255 . round $ theCalc kab kdb ksb alph
    in
        color intense_r intense_g intense_b

colorTriangles :: Material -> VertNorms -> [Triangle Double]
    -> M.Map (Int,Int) Color
colorTriangles mat vn = M.unions . map (colorTriangle mat vn)

colorTriangle :: Material -> VertNorms -> Triangle Double
    -> M.Map (Int,Int) Color
colorTriangle mat vn (Triangle (a, b, c)) = let
    [bot, mid, top] =
        L.sortOn (getY.fst) [(a,vn M.! a), (b,vn M.! b), (c,vn M.! c)]
    e1 = --trace (show [bot, mid, top]) $
            vecterpolateY bot top
    e2 = vecterpolateY mid top ++ (tail $ vecterpolateY bot mid)
    es = if 2*(getX.fst $ mid) <= ((getX.fst $ top) + (getX.fst $ bot))
            then zip e2 e1 else zip e1 e2
    -- we need to get the vertex normals before this step
    in M.fromList . (map.mapSnd) (colorNormal mat) $
        concatMap (uncurry vecterpolateX) es

plotPxs :: (MonadState ZBuf m) => [Pixel] -> m [Pixel]
plotPxs pxs = do
    zb <- get
    let ok = [p | (Pixel p) <- pxs, inRange (bounds zb) (fst p),
                                zb!(fst p) > snd p]
    modify $ modZB ok
    return $! map Pixel ok

