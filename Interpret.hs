{-# LANGUAGE FlexibleContexts #-}
module Interpret where

import Parser
import Line
import Screen
import DrawMats
import Lighting
import qualified Solids as S
import qualified Transform as T

import Text.Printf
import System.IO
import System.Process
import System.Directory
import System.Environment
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.ByteString.Lazy as BS

----   STUFF THaT MAYBE SHOULD NOT BE HERE  ----

-- lol

findFrames :: [Command] -> Frame
findFrames cmds =
    case ([ round x | CmdFrames x <- cmds ]) of
        [] -> error "no frames command found, stopping..."
        [ok] -> ok
        _  -> error "multiple frams commands found, stopping..."

-- I just want this to work god damn
findFramesName :: [Command] -> DrawMats
findFramesName cmds = let
    f = case ([ round x | CmdFrames x <- cmds ]) of
        [] -> error "no frames command found, stopping..."
        [ok] -> ok
        _  -> error "multiple frams commands found, stopping..."
    nam = case ([ n | CmdBasename n <- cmds ]) of
        [] -> "anim"
        [ok] -> ok
        _ -> error "multiple basenames found, stopping..."
    in emptyDM { numFrames = f, baseName = nam }

setupKnobs :: [Command] -> DrawMats
setupKnobs cmds = execState (mapM_ godSneezeForVary cmds) (findFramesName cmds)

interpretFrame :: MonadReader Frame m => [Command] -> m DrawMats
interpretFrame cmds = execStateT (interpret cmds) (setupKnobs cmds)

saveFrame :: (MonadReader Frame m, MonadIO m) => DrawMats -> m ()
saveFrame dm = do
    fr <- ask
    let path = "anim/" ++ baseName dm ++ (printf "_%04d.png" fr)
    liftIO $ do
        BS.writeFile ".tempimg.ppm" (printPixels . downsample $ getScreen dm)
        callProcess "convert" [".tempimg.ppm", path]
        putStr $ printf "rendered frame %4d\n" fr
        removeFile ".tempimg.ppm"

render :: (MonadIO m) => [Command] -> m ()
render cmds = do
    let fs = numFrames  $ findFramesName cmds
        nm = baseName   $ findFramesName cmds
    (mapM_ . runReaderT $ interpretFrame cmds >>= saveFrame) [0..fs-1]
    liftIO $ do
        putStrLn "\nrenderinc complete, animating..."
        callProcess "convert" ["-delay", "1.7", "anim/"++nm++"*", nm++".gif"]
        callProcess "eog" [nm++".gif"]

----END STUFF TTHAT MAYBE SHOULD NOT BE HERE----

-- we gotta make em all monadreaders

interpret :: (MonadState DrawMats m, MonadReader Frame m) => [Command] -> m ()
interpret = mapM_ cmd

godSneezeForVary :: MonadState DrawMats m => Command -> m ()
godSneezeForVary c = case c of
    CmdVary s a b c d   -> vary s (round a) (round b) c d
    _                   -> return ()

cmd :: (MonadReader Frame m, MonadState DrawMats m) => Command -> m ()
cmd c = case c of
    CmdPush                 -> push
    CmdPop                  -> pop
    CmdBox      a b c d     -> box      a b c d 
    CmdSphere   a b c d     -> sphere   a b c d 
    CmdTorus    a b c d e   -> torus    a b c d e
    CmdLine     a b c d e   -> line     a b c d e
    CmdMove     a b         -> move     a b
    CmdScale    a b         -> scale    a b
    CmdRotate   a b c       -> rote     a b c
    CmdConstants s r g b i a-> constants s r g b i a
    CmdVary s a b c d       -> vary s (round a) (round b) c d
--  CmdDisplay              -> display
--  CmdSave path            -> save path
    _                       -> return ()

vary :: (MonadState DrawMats m) =>
    String -> Frame -> Frame -> Double -> Double -> m ()
vary name sT eT sV eV = modify $ appendKnob name sT eT sV eV

constants :: (MonadState DrawMats m) =>
    String -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Db -> m ()
constants name (kar,kdr,ksr) (kag,kdg,ksg) (kab,kdb,ksb) (ir,ig,ib) alph =
    modify $ addMaterial name m
        where m = Material kar kdr ksr kag kdg ksg kab kdb ksb ir ig ib alph

--save :: (MonadState DrawMats m, MonadIO m) => String -> m ()
--save path = do
--    dm <- get
--    liftIO $ do
--        writeFile ".tempimg.ppm" (printPixels {-. downsample-} $ getScreen dm)
--        callProcess "convert" [".tempimg.ppm", path]
--        removeFile ".tempimg.ppm"
--
--display :: (MonadState DrawMats m, MonadIO m) => m ()
--display = do
--    dm <- get
--    liftIO $ do
--        writeFile ".tempimg.ppm" (printPixels {-. downsample-} $ getScreen dm)
--        callProcess "eog" [".tempimg.ppm"]
--        removeFile ".tempimg.ppm"

rote :: (MonadReader Frame m, MonadState DrawMats m) => Axis -> Db -> MS -> m ()
rote ax theta k = do
    f <- ask
    dm <- get
    let sc = case k of 
            Just name -> findKnob name dm f
            Nothing   -> 1
        roti ax theta
            | ax == AxisX = T.rotX (-theta)
            | ax == AxisY = T.rotY (-theta)
            | ax == AxisZ = T.rotZ (-theta)
    modify . modTransform $ (mappend $ roti ax (sc*theta))

scale :: (MonadReader Frame m, MonadState DrawMats m) => Vec3 -> MS -> m ()
scale (x,y,z) k = do
    f <- ask
    dm <- get
    let sc = case k of 
            Just name -> findKnob name dm f
            Nothing   -> 1
    modify . modTransform $ (mappend $ T.scale (sc*x) (sc*y) (sc*z))

move :: (MonadReader Frame m, MonadState DrawMats m) => Vec3 -> MS -> m ()
move (x,y,z) k = do
    f <- ask
    dm <- get
    let sc = case k of 
            Just name -> findKnob name dm f
            Nothing   -> 1
    modify . modTransform $ (mappend $ T.trans (sc*x) (sc*y) (sc*z))

line :: (MonadState DrawMats m) => MS -> Vec3 -> MS -> Vec3 -> MS -> m ()
line _ (x0,y0,z0) _ (x1,y1,z1) _ = do
    dm <- get
    let ln = Line (T.pmult (getTransform dm) (Vect x0 y0 z0 1))
                  (T.pmult (getTransform dm) (Vect x1 y1 z1 1))
    modify . modScreen $ drawLine red (fmap round ln)

box :: (MonadState DrawMats m) => MS -> Vec3 -> Vec3 -> MS -> m ()
box mat (cx,cy,cz) (w,h,d) _ = do
    dm <- get
    let (vn,tris) = S.boxNormals cx cy cz w h d
        m = case mat of
            Nothing -> defaultMat
            Just s  -> findMaterial s dm
    drawTriangles m (S.trVNs (getTransform dm) vn) (trTris dm tris)

sphere :: (MonadState DrawMats m) => MS -> Vec3 -> Db -> MS -> m ()
sphere mat (cx,cy,cz) r _ = do
    dm <- get
    let (vn,tris) = S.sphereNormals cx cy cz r
        m = case mat of
            Nothing -> defaultMat
            Just s  -> findMaterial s dm
    drawTriangles m (S.trVNs (getTransform dm) vn) (trTris dm tris)

torus :: (MonadState DrawMats m) => MS -> Vec3 -> Db -> Db -> MS -> m ()
torus mat (cx,cy,cz) r0 r1 _ = do
    dm <- get
    let (vn,tris) = S.torusNormals cx cy cz r0 r1
        m = case mat of
            Nothing -> defaultMat
            Just s  -> findMaterial s dm
    drawTriangles m (S.trVNs (getTransform dm) vn) (trTris dm tris)

push :: (MonadState DrawMats m) => m ()
push = modify pushTransform

pop :: (MonadState DrawMats m) => m ()
pop = modify popTransform

