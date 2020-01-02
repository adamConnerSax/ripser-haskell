{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Numeric.Ripser                as R
import qualified Numeric.LinearAlgebra         as LA
import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Data.Text                     as T

import qualified Control.Foldl                 as FL

import qualified System.IO                     as S
import qualified System.Random                 as SR
-- this assumes ripser directory, with working executable, and examples dir
-- is located at "ripser"

import qualified Graphics.Vega.VegaLite        as GV
import qualified Knit.Report                   as K

templateVars :: M.Map String String
templateVars = M.fromList
  [ ("lang"     , "English")
  , ("author"   , "Adam Conner-Sax")
  , ("pagetitle", "ripser-haskell test")
  , ("tufte"    , "True")
  ]

main :: IO ()
main = do
  S.hSetBuffering S.stdout S.NoBuffering
  let pathToRipser   = "ripser/ripser"
      pathToExamples = "ripser/examples/"

  let template = K.FromIncludedTemplateDir "mindoc-pandoc-KH.html"
  pandocWriterConfig <- K.mkPandocWriterConfig template
                                               templateVars
                                               K.mindocOptionsF

  let knitConfig = K.defaultKnitConfig
        { K.outerLogPrefix     = Just "ripserTest.Main"
        , K.logIf              = K.logAll
        , K.pandocWriterConfig = pandocWriterConfig
        }
  resE <- K.knitHtml knitConfig $ makeDoc pathToExamples pathToRipser
  case resE of
    Right htmlAsText ->
      K.writeAndMakePathLT "test/html/ripserTest.html" htmlAsText
    Left err -> putStrLn $ "Pandoc Error: " ++ show err

makeDoc :: K.KnitOne r => T.Text -> T.Text -> K.Sem r ()
makeDoc pathToExamples pathToRipser = do
  random16LD <-
    K.liftKnit
    $  R.lowerDistanceMatrixFromFile
    $  T.unpack pathToExamples
    ++ "random16.lower_distance_matrix"
  random16Barcodes <- K.liftKnit $ R.callRipser (Just $ T.unpack pathToRipser)
                                                Nothing
                                                Nothing
                                                Nothing
                                                random16LD
  K.logLE K.Info
    $  "Output:\n"
    <> (T.pack . L.intercalate "\n" $ fmap show random16Barcodes)
  sphere_3_192LD <-
    K.liftKnit
    $  R.lowerDistanceMatrixFromFile
    $  T.unpack pathToExamples
    ++ "sphere_3_192.lower_distance_matrix"
  sphere_3_192Barcodes <- K.liftKnit $ R.callRipser
    (Just $ T.unpack pathToRipser)
    Nothing
    Nothing
    Nothing
    sphere_3_192LD
  K.logLE K.Info
    $  "Output:\n"
    <> (T.pack $ L.intercalate "\n" $ fmap show sphere_3_192Barcodes)


  -- sample from an annulus
  let innerRadius = 0.9 :: Double
      outerRadius = 1.0 :: Double
      n           = 200
  (g1, g2) <- SR.split <$> K.liftKnit SR.getStdGen
  let rs          = SR.randomRs (innerRadius, outerRadius) g1
      angles      = SR.randomRs (0 :: Double, 2 * pi) g2
      polarCoords = zip (take n rs) (take n angles)
      fromPolar (r, th) = (r * cos th, r * sin th)
      xyCoords = fmap fromPolar polarCoords
      radius x y = sqrt $ x * x + y * y
      euclid (x1, y1) (x2, y2) = radius (x1 - x2) (y1 - y2)
      ripserInput = R.distanceMatrix euclid xyCoords
{-  K.logLE K.Info
    $  "polar\n"
    <> T.pack (show polarCoords)
    <> "\nXY\n"
    <> (T.pack . show $ xyCoords)
    <> "\nLDM\n"
    <> T.pack (show ripserInput) -}
  annulusBarcodesLDM <- K.liftKnit $ R.callRipser
    (Just $ T.unpack pathToRipser)
    Nothing
    Nothing
    Nothing
    ripserInput
  K.logLE K.Info
    $  "Output (input as LDM):\n"
    <> (T.pack $ L.intercalate "\n" $ fmap show annulusBarcodesLDM)
  _ <- K.addHvega Nothing Nothing $ plotXY
    (  "Inner="
    <> (T.pack $ show innerRadius)
    <> "; outer="
    <> (T.pack $ show outerRadius)
    )
    id
    xyCoords

  annulusBarcodesPC <- K.liftKnit $ R.callRipser
    (Just $ T.unpack pathToRipser)
    Nothing
    Nothing
    Nothing
    (R.PointCloud $ LA.fromRows $ fmap (\(x, y) -> LA.fromList [x, y]) xyCoords)

  K.logLE K.Info
    $  "Output (input as PC):\n"
    <> (T.pack $ L.intercalate "\n" $ fmap show annulusBarcodesPC)

  return ()

plotXY :: Foldable f => T.Text -> (a -> (Double, Double)) -> f a -> GV.VegaLite
plotXY title getXY rows =
  let datRow r =
        let (x, y) = getXY r
        in  GV.dataRow [("X", GV.Number x), ("Y", GV.Number y)] []
      datRows =
        GV.dataFromRows [] $ concat $ fmap datRow $ FL.fold FL.list rows
      encX = GV.position GV.X [GV.PName "X", GV.PmType GV.Quantitative]
      encY = GV.position GV.Y [GV.PName "Y", GV.PmType GV.Quantitative]
      enc  = GV.encoding . encX . encY
  in  GV.toVegaLite [enc [], GV.mark GV.Point [], datRows]
