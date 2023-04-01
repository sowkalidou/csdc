{-# LANGUAGE OverloadedStrings #-}

module CSDC.Image
  ( generateImageFromName,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Text (Text)
import Data.Text qualified as Text
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg.Core
import System.Random

-- Colors from Bulma
colours :: [Colour Double]
colours =
  map
    (uncurryRGB sRGB)
    [ hsl 171 1 0.41,
      hsl 217 71 0.53,
      hsl 204 0.86 0.53,
      hsl 141 0.53 0.53,
      hsl 48 1 0.67,
      hsl 348 1 0.61
    ]

randomColour :: IO (Colour Double)
randomColour = do
  let n = length colours
  r <- randomRIO (0, n - 1)
  return $ colours !! r

lightGray :: Colour Double
lightGray = uncurryRGB sRGB $ hsl 0 0 0.96

profileDiagram :: Text -> Colour Double -> Diagram B
profileDiagram name colour =
  let base =
        fc colour $
          lw none $
            square 1

      letter =
        translate (r2 (-0.01, -0.09)) $
          font "Helvetica" $
            fc lightGray $
              text $
                Text.unpack $
                  Text.take 1 name
   in letter `atop` scale 1.2 base

renderDiagram :: Diagram B -> ByteString
renderDiagram =
  renderBS
    . renderDia SVG (SVGOptions (mkWidth 96) Nothing "" [] True)

generateImageFromName :: Text -> IO ByteString
generateImageFromName name = do
  colour <- randomColour
  let diagram = profileDiagram name colour
  pure $ renderDiagram diagram
