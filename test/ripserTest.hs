{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Numeric.Ripser                as R
import qualified Data.List                     as L
import qualified System.IO                     as S
-- this assumes ripser directory, with working executable, and examples dir
-- is located at "ripser"

main :: IO ()
main = do
  S.hSetBuffering S.stdout S.NoBuffering
  let pathToRipser   = "ripser/ripser-debug"
      pathToExamples = "ripser/examples/"
  random16LD <-
    R.lowerDistanceMatrixFromFile
    $  pathToExamples
    ++ "random16.lower_distance_matrix"
--  putStrLn $ "loaded input: \n" ++ show random16LD
  random16Barcodes <- R.callRipser (Just pathToRipser)
                                   Nothing
                                   Nothing
                                   Nothing
                                   random16LD
  putStrLn $ "Output:\n" ++ (L.intercalate "\n" $ fmap show random16Barcodes)
  sphere_3_192LD <-
    R.lowerDistanceMatrixFromFile
    $  pathToExamples
    ++ "sphere_3_192.lower_distance_matrix"
--  putStrLn $ "loaded input: \n" ++ show sphere_3_192LD -}
  sphere_3_192Barcodes <- R.callRipser (Just pathToRipser)
                                       Nothing
                                       Nothing
                                       Nothing
                                       sphere_3_192LD
  putStrLn
    $  "Output:\n"
    ++ (L.intercalate "\n" $ fmap show sphere_3_192Barcodes)
