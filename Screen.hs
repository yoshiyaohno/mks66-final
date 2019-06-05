{-# LANGUAGE BinaryLiterals, OverloadedStrings #-}
module Screen where

import Data.Int
import Data.Bits
import Data.Word
import Data.Array.Unboxed
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSW8

type ZBuf   = UArray (Int, Int) Double
type Screen = UArray (Int, Int) Color
type Color  = Int32

draw :: [((Int,Int), Color)] -> Screen -> Screen 
draw l s = s // (filter (inRange ((x0,y0), (x1-1, y1-1)) . fst) l)
    where ((x0,y0), (x1,y1)) = bounds s

modZB :: [((Int, Int), Double)] -> ZBuf -> ZBuf
modZB l s = s // (filter (inRange ((x0,y0), (x1-1, y1-1)) . fst) l)
    where ((x0,y0), (x1,y1)) = bounds s

wht = color 255 255 255
blk = color 0 0 0
red = color 255 0 0
blu = color 0 0 255
grn = color 0 255 0

showC :: Color -> String
showC = unwords . map show . ([getR, getG, getB] <*>) . pure

downsample :: Screen -> Screen
downsample s = array ((0,0), (w `div` 2, h `div` 2))
    [((div x 2, div y 2),
        colorAvg [s!(x,y), s!(x+1,y), s!(x,y+1), s!(x+1,y+1)])
        | x <- [0,2..w-1], y <- [0,2..h-1]]
            where ((0,0),(w,h)) = bounds s

intAvg :: (Integral a) => [a] -> a
intAvg l = sum l `div` (fromIntegral $ length l)

colorAvg :: [Color] -> Color
colorAvg cs = color (jef getR) (jef getG) (jef getB)
    where jef f = (intAvg $ map f cs)

{-# INLINE color #-}
color :: Color -> Color -> Color -> Color
color r g b = r `shiftL` 16 + g `shiftL` 8 + b

{-# INLINE getR #-}
getR :: Color -> Color
getR = (`shiftR` 16)

{-# INLINE getG #-}
getG :: Color -> Color
getG = (.&. 0xff) . (`shiftR` 8)

{-# INLINE getB #-}
getB :: Color -> Color
getB = (.&. 0xff)

emptyZB :: (Int, Int) -> ZBuf
emptyZB (w,h) = array ((0,0),(w,h))
        [((x,y), 2**1024) | x <- [0..w], y <- [0..h]]

emptyScreen :: Color -> (Int, Int) -> Screen
emptyScreen c (w,h) =
    array ((0,0), (w,h)) [((x,y), c) | x <- [0..w], y <- [0..h]]

getRGB :: Color -> [Word8]
getRGB c = map fromIntegral [getR c, getG c, getB c]

-- takes a screen and puts in ppm format
printPixels :: Screen -> BS.ByteString
printPixels scrn =
    ppmHeader (w1-w0, h1-h0)
    `BS.append` (BS.pack $ getRGB =<<
        [scrn!(x, y) | x <- [w0..w1-1], y <- reverse [h0..h1-1]])
            where ((w0,h0), (w1,h1)) = bounds scrn
       
-- why
showToBS :: (Show a) => a -> BS.ByteString
showToBS = BSW8.pack . show
 
ppmHeader :: (Int, Int) -> BS.ByteString
ppmHeader (w, h) = BS.concat ["P6 ", showToBS w, " ", showToBS h, " 255\n"]

