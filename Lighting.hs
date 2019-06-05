{-# LANGUAGE FlexibleContexts #-}
module Lighting where

import Line
import Screen
import Solids
import DrawMats

import Data.Array.Unboxed
import Control.Monad.State
import qualified Data.Set as S

drawTriangle :: (MonadState DrawMats m) => Material -> Triangle Double -> m ()
drawTriangle mat tr = do
    dm <- get
    let pxs = scanTriangle tr
        (toDraw, newZB) = runState (plotPxs pxs) (getZBuf dm)
        c = colorTriangle mat tr
    put $ dm {getZBuf = newZB}
    modify.modScreen $ draw [((pgetX p, pgetY p), c) | p <- S.toList toDraw]

drawTriangles :: (MonadState DrawMats m) => Material -> [Triangle Double] -> m ()
drawTriangles mat = mapM_ (drawTriangle mat) . bfCull

colorTriangle :: Material -> Triangle Double -> Color
--hardcoded light moments
colorTriangle (Material kar kdr ksr kag kdg ksg kab kdb ksb ir ig ib alph) tr =
    let norm = normalize $ normal tr
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

plotPxs :: (MonadState ZBuf m) => S.Set Pixel -> m (S.Set Pixel)
plotPxs pxs = do
    zb <- get
    let ok = [p | (Pixel p) <- S.toList pxs, inRange (bounds zb) (fst p),
                                zb!(fst p) > snd p]
    modify $ modZB ok
    return $ S.fromList $ map Pixel ok

