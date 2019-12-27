{-# LANGUAGE OverloadedStrings #-}
module Numeric.Ripser
  ()
where

import qualified System.Process.Typed          as SP
import qualified System.IO                     as S
import qualified System.IO.Error               as S
import qualified Control.Exception             as X
import qualified Numeric.LinearAlgebra         as LA
import qualified Control.Foldl                 as FL
import qualified Data.Interval                 as I
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe
                                                , maybe
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
import qualified Data.Text.IO                  as T
import qualified Data.Vector.Storable          as VS
import qualified Data.ByteString.Lazy.         as BL
import           Control.Concurrent.STM         ( atomically )

import qualified  Text.MegaParsec as P
import qualified Text.MegaParsec.Byte as P
import qualified Text.MegaParsec.Byte.Lexer as PL

data Input = LowerDistance (LA.Matrix Double) | PointCloud (LA.Matrix Double) | Sparse [(Int, Int, Double)]
type Dimension = Int -- specify max dimension for computed persistent homology
type Threshold = Double -- only compute Rips complexes up to this diameter
type Ratio = Double -- only report persistence pairs with death/birth > Ratio

type Distance a = (a -> a -> Double) -- should be >= 0 for all a

data PersistenceInterval = PersistenceInterval { dim :: Int, interval :: I.Interval Double }

callRipser
  :: Maybe FilePath
  -> Maybe Dimension
  -> Maybe Threshold
  -> Maybe Ratio
  -> Input
  -> IO [PersistenceInterval]
callRipser pathM dimM threshM ratioM input = do
  let ripserPath = fromMaybe "ripser" pathM
      optFromMaybe optS vM = maybe "" (\v -> optS <> (T.pack $ show v)) vM
      dimOpt    = optFromMaybe "--dim=" dimM
      threshOpt = optFromMaybe "--thresh" threshM
      ratioOpt  = optFromMaybe "--ratio" ratioM
      inputOpt  = "--format=" <> case input of
        LowerDistance _ -> "lower-distance"
        PointCloud    _ -> "point-cloud"
        Sparse        _ -> "sparse"
      opts    = fmap T.unpack [dimOpt, threshOpt, ratioOpt, inputOpt]
      encoded = TL.encodeUtf8 $ TL.fromStrict $ encodeInput input
      ripserProc =
        SP.setStdin (SP.byteStringInput encoded)
          $ SP.setStdout SP.byteStringOutput
          $ SP.proc ripserPath opts
  SP.withProcessWait_ ripserProc $ \p -> do
    unparsed <- atomically (SP.getStdout p)
    case parseOutput unparsed of
      Left parseErr ->
        X.throwIO $ S.userError "parse failure on output of ripser."
      Right x -> return x

encodeInput :: Input -> T.Text
encodeInput (LowerDistance mLD) =
  let (rows, cols) = LA.size mLD
  in  T.intercalate " " $ fmap (T.pack . show) $ do
        c <- [0 .. (cols - 1)]
        r <- [c .. (rows - 1)]
        return $ mLD `LA.atIndex` (r, c)

encodeInput (PointCloud mPC) =
  let rowToCSV = T.intercalate "," . fmap (T.pack . show) . VS.toList
  in  T.intercalate "\n" $ fmap rowToCSV $ LA.toRows mPC

encodeInput (Sparse lSP) = T.intercalate "\n" $ fmap
  (\(i, j, d) ->
    (T.pack $ show i) <> " " <> (T.pack $ show j) <> " " <> (T.pack $ show d)
  )
  lSP

type Parser a = Parsec Void BL.ByteString

parseOutput :: BL.ByteString -> Either T.Text [PersistenceInterval]
parseOutput ro = either (T.pack . p.errorBundlePretty) id $ P.runParser p "ripser-output" ro where
  let ignoreLineStarting :: BL.ByteString -> Parser ()
      ignoreLineStarting starts = do
        P.void $ P.string starts        
        _ <- P.manyTill PL.charLiteral eol
      parseDimLine :: Parser Int
      parseDimLine = do
        P.void $ P.string "persistence intervals in dim "
        P.lexeme PL.decimal
      parseInterval :: Parser PersistenceInterval
      parseInterval = do
  p :: Parser [PersistenceInterval]
  p = do
    ignoreLineStarting ""
    ignoreLineStarting ""
    parseDimLine
    
    
    

distanceMatrix
  :: Foldable f
  => Distance a -- ^ distance function
  -> f a -- ^ items
  -> Input -- ^ Lower Triangualar Distance Matrix
distanceMatrix d items = LowerDistance (LA.fromColumns $ go itemsL)
 where
  itemsL = FL.fold FL.list items
  n      = L.length items
  oneRow [] = VS.replicate n 0
  oneRow (x : xs) =
    VS.fromList $ L.replicate (n - L.length xs) 0 ++ fmap (d x) xs
  go []       = []
  go (x : xs) = oneRow (x : xs) : go xs


sparseDistanceMatrix
  :: Foldable f
  => Distance a -- ^ distance function
  -> Double -- ^ threshold for removing from sparse representation
  -> f a -- ^ items
  -> Input
sparseDistanceMatrix = undefined


