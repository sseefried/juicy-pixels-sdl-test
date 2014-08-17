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
import           System.Time
import           Data.IORef
import           Text.Printf

screenWidth  = 512
screenHeight = 512

foreign export ccall "haskell_mainSDL" mainSDL :: IO ()

----------------------------------------------------------------------
image :: Image PixelRGBA8
image = Image { imageWidth = w, imageHeight = h, imageData = img  }
  where
    w = screenWidth
    h = screenHeight
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

imagePtr = fst . unsafeToForeignPtr0 . imageData

mainSDL = S.withInit [S.InitVideo] $
    do screen <- S.setVideoMode screenWidth screenHeight bitsPerPixel [S.HWSurface]
       S.setCaption "Test" ""
       S.enableUnicode True
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
       t <- getClockTime
       tRef <- newIORef 0
       let draw = display surface tRef
       draw
       loop t tRef draw

display :: S.Surface -> IORef Int -> IO ()
display surface tRef
    = do screen <- S.getVideoSurface
         -- blit from JuicyPixels 'image' to surface
         S.blitSurface surface Nothing screen (Just (S.Rect 0 0 0 0))
         S.flip screen
         modifyIORef tRef (+1)

loop :: ClockTime -> IORef Int -> IO () -> IO ()
loop t tRef display
    = do let quit = do
               t' <- getClockTime
               n <- readIORef tRef
               let td = diffClockTimes t' t
               let diffToSec td = fromIntegral (tdMin td) * 60.0 +
                                  fromIntegral (tdSec td) +
                                  fromIntegral (tdPicosec td) / 1e12

               printf "frames = %d, elapsed time = %f\n" n (diffToSec td :: Float)
               printf "Framerate = %.2f frames/s\n" (fromIntegral n / diffToSec td :: Float)
               exitWith ExitSuccess
         event <- S.pollEvent
         case event of
           S.Quit -> quit
           S.KeyDown (S.Keysym _ _ 'q') -> quit
           _ -> display
         loop t tRef display
