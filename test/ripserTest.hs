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
  putStrLn $ "loaded input: \n" ++ show random16LD
{-  sphere_3_192LD <-
    R.lowerDistanceMatrixFromFile
    $  pathToExamples
    ++ "sphere_3_192.lower_distance_matrix"
  putStrLn $ "loaded input: \n" ++ show sphere_3_192LD -}
  ripserOutput <- R.callRipser (Just pathToRipser)
                               Nothing
                               Nothing
                               Nothing
                               random16LD
  putStrLn $ "Output:\n" ++ (L.intercalate "\n" $ fmap show ripserOutput)

