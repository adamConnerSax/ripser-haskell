{-# LANGUAGE OverloadedStrings #-}
module Numeric.Ripser
  ()
where

import qualified System.Process.Typed          as SP
import qualified System.IO                     as S
import qualified Control.Exception             as X
import qualified Numeric.LinearAlgebra         as LA
import qualified Control.Foldl                 as FL
import qualified Data.Interval                 as I
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.IO                  as T
import qualified Vector.Storable               as VS
import qualified Data.ByteString.Lazy.Char8    as LBC
import           Control.Concurrent.STM (atomically)

data Input = LowerDistance LA.MatrixDouble | PointCloud (LA.Matrix Double) | Sparse [(Int, Int, Double)]
type Dimension = Int -- specify max dimension for computed persistent homology
type Threshold = Double -- only compute Rips complexes up to this diameter
type Ratio = Double -- only report persistence pairs with death/birth > Ratio

type Distance a = (a -> a -> Double) -- should be >= 0 for all a

data PersistenceInterval = PersistenceInterval { dim :: Int, interval :: I.Interval Double }

callRipser
  :: Maybe FilePath
  -> Maybe Dimenion
  -> Maybe Threshold
  -> Maybe Ratio
  -> Input
  -> IO [PersistenceInterval]
callRipser pathM dimM threshM ratioM input =
  let ripserPath = maybe "ripser" pathM
      optFromMaybe optS vM = maybe "" (\v -> optS <> (T.pack $ show v)) vM
      dimOpt = optFromMaybe "--dim=" dimM
      threshOpt = optFromMaybe "--thresh" threshM
      ratioOpt = optFromMaybe "--ratio" ratioM
      inputOpt = "--format=" <> case Input of
                                  LowerDIstance _ -> "lower-distance"
                                  PointCloud _ -> "point-cloud"
                                  Sparse _ -> "sparse"
      opts = fmap T.unpack [dimOpt, threshOpt, ratioOpt, inputOpt]
      ripserProc = SP.proc ripserPath opts
  in SP.withProcessWait_ ripserProc $ \p -> do
    T.hPutStr (SP.getStdin p) $ encodeInput
    S.hClose (SP.getStdin p)
    SP.atomically (SP.getStdout p) >>= parseOutput 

encodeInput :: Input -> TL.Text
encodeInput = undefined

parseOutput :: LBC.ByteString -> Either T.Text [PersistenceInterval]
parseOutput = undefined

distanceMatrix
  :: Foldable f
  => Distance a -- ^ distance function
  -> f a -- ^ items
  -> Input -- ^ Lower Triangualar Distance Matrix
distanceMatrix d items = LowerDistance (LA.fromColums $ go itemsL)
 where
  itemsL = FL.fold FL.list items
  n      = L.length items
  oneRow [] = VS.replicate n 0
  oneRow (x : xs) =
    VS.fromList $ L.replicate (n - L.length xs) 0 ++ fmap (d x) xs
  go []       = oneRow []
  go (x : xs) = oneRow (x : xs) : go xs


sparseDistanceMatrix
  :: Foldable f
  => Distance a -- ^ distance function
  -> Double -- ^ threshold for removing from sparse representation
  -> f a -- ^ items
  -> SparseDistance
sparseDistanceMatrix = undefined


