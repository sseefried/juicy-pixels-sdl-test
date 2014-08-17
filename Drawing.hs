{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Drawing where

import Graphics.Rendering.Cairo (Render)
import Diagrams.Prelude hiding (Render, p2)
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

screenWidth, screenHeight :: Int
screenWidth  = 600
screenHeight = 600

diagramRender :: Diagram Cairo R2 -> Render ()
diagramRender d =
  let w   = width d
      h   = height d
      wid = fromIntegral screenWidth * min (w/h) 1
  in snd $ renderDia Cairo (CairoOptions "never_produced.png" (Width wid) PNG False) d




---------------------------------------------------------------------------

illustrateEnvelope :: (Enveloped a, V a ~ R2) =>
                       R2 -> a -> Diagram B R2
illustrateEnvelope v d
  = mconcat
    [ arrowAt' (with & arrowHead .~ tri) origin v'
    , origin ~~ b # lc green # lw veryThick
    , p1 ~~ p2 # lc red
    ]
    where
      b  = envelopeP v d
      v' = normalized v
      p1 = b .+^ (rotateBy (1/4) v')
      p2 = b .+^ (rotateBy (-1/4) v')

d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11 :: Diagram Cairo R2

d1 = bg white (stroke d <> illustrateEnvelope (r2 (0.4,-0.1)) d)
 where
  d :: Path R2
  d = circle 1 ||| circle 0.3

d4 = roundedRect' 0.7 0.4 (with & radiusTL .~ 0.1
                                & radiusTR .~ -0.2
                                & radiusBL .~ 0.3)

d5 = polygon (with & polyType .~ PolyRegular 7 1)

theSq = square 1 # lwN 0.01


d2 = hcat' (with & sep .~ 0.2)
      (map (\s -> theSq # scale s) [0.5, 0.8, 1, 1.5, 2])
      # scale 20

d3 = circle 0.5 <> unitCircle

d6 = let n = 40 in
     polygon ( with & polyType  .~ PolyPolar (repeat (1/(fromIntegral n) @@ turn))
                                              (take n $ cycle [1,2,4,2]) )

d7 = beside (r2 (20,30))
                  (circle 1 # fc orange)
                  (circle 1.5 # fc purple)
           # showOrigin

d8 = circles # showOrigin ||| strutX 1 ||| ((circles <> square 1) # showOrigin)
  where
    circles = mconcat [d1, d2, d3]
    d1      = juxtapose unitX             (square 1) (circle 1 # fc red)
    d2      = juxtapose (unitX ^+^ unitY) (square 1) (circle 1 # fc green)
    d3      = juxtapose unitY             (square 1) (circle 1 # fc blue)

d9 = hcat' (with & sep .~ 1 ) . take 4 . iterate (opacity 0.7) $ reds
  where
    s c =  square 1 # fc c
    reds    = (s darkred ||| s red) === (s pink ||| s indianred)

d10 = circle 0.35 # fillTexture radial # lw none <>
      rect 2 1 # fillTexture linear # lw none
  where
    radial = mkRadialGradient (mkStops [(white,0,1), (black,1,1)])
                           ((-0.15) ^& (0.15)) 0.06 (0 ^& 0) 0.5
                           GradPad

    linear = mkLinearGradient (mkStops [(black,0,1), (white,1,1)])
                           (0 ^& (-0.2)) (0 ^& 0.2)
                           GradPad

d11 = hcat . map (eff #) $ rs
 where
  eff = text "F" <> square 1 # lw none
  rs  = map rotateBy [1/7, 2/7 .. 6/7]

d12 = hcat . map (eff #) $ ts
 where
  eff = text "F" <> square 1 # lw none
  ts  = [ scale (1/2), id, scale 2,    scaleX 2,    scaleY 2
        ,                  scale (-1), scaleX (-1), scaleY (-1)
        ]



theDrawing :: Render ()
theDrawing = diagramRender (bg white d12)

-----------------------------------------------------------------