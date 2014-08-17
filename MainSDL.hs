{-# LANGUAGE ForeignFunctionInterface #-}
module MainSDL where

import qualified Graphics.UI.SDL as S
import           Foreign.Ptr (castPtr, Ptr(..))
import           Foreign.ForeignPtr (castForeignPtr, ForeignPtr(..), withForeignPtr)
import           System.Exit
import           System.Random
import           Data.Vector.Storable (Vector(..), unsafeToForeignPtr0)
import qualified Data.Vector.Storable as V
import           GHC.Word
import           Codec.Picture



screenWidth = 512
screenHeight = 512

foreign export ccall "haskell_mainSDL" mainSDL :: IO ()

----------------------------------------------------------------------

image :: Image PixelRGBA8
image = Image { imageWidth = w, imageHeight = h, imageData = img  }
  where
    w = 512
    h = 512
    img :: Vector Word8
    img = V.generate (w*h*4) f
    f :: Int -> Word8
    f x = let n = x `div` 4
              r = x `mod` 4
          in if r == 3 then 0xFF else if r `elem` [0] then fracOf n else 0
    fracOf n = floor (0xFF * fromIntegral n/(fromIntegral (w*h)))
----------------------------------------------------------------------

--
-- You need to do a few things to write a Haskell SDL application
-- 1. Add the LANGUAGE ForeignFunctionInterface pragma
-- 2. foreign export ccall "haskell_mainSDL" <your_main> :: IO ()
-- 3. Compile with GHC with "-main-is" option.
-- 4. Link against mainc.o
--

bitsPerPixel  = 32
bytesPerPixel = bitsPerPixel `div` 8

mainSDL = S.withInit [S.InitVideo] $
    do screen <- S.setVideoMode screenWidth screenHeight bitsPerPixel [S.SWSurface]
       S.setCaption "Test" ""
       S.enableUnicode True
       display
       loop display

-- unsafeToForeignPtr0 :: Storable a => Vector a -> (ForeignPtr a, Int)


imagePtr :: Image PixelRGBA8 -> ForeignPtr Word8
imagePtr im = fst $ unsafeToForeignPtr0 $ imageData im


display :: IO ()
display
    = do screen <- S.getVideoSurface
         let format = S.surfaceGetPixelFormat screen
         white <- S.mapRGB format 0xFF 0xFF 0xFF
         S.fillRect screen Nothing white
         -- blit from JuicyPixels 'image' to surface
         surface <- withForeignPtr (imagePtr image) $ \ptr -> S.createRGBSurfaceFrom
                      ptr
                      screenWidth
                      screenHeight
                      bitsPerPixel
                      (screenWidth * bytesPerPixel)
                      0x000000FF
                      0x0000FF00
                      0x00FF0000
                      0xFF000000
         S.blitSurface surface Nothing screen (Just (S.Rect 0 0 0 0))
         S.flip screen

loop :: IO () -> IO ()
loop display
    = do event <- S.waitEvent
         case event of
           S.Quit -> exitWith ExitSuccess
           S.KeyDown (S.Keysym _ _ 'q') -> exitWith ExitSuccess
           S.KeyDown (S.Keysym _ _ ' ') -> display
           _ -> return ()
         loop display