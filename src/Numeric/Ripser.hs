{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Numeric.Ripser
  ( Input(..)
  , Dimension
  , Threshold
  , Ratio
  , Distance
  , PersistenceInterval(..)
  , callRipser
  , lowerDistanceMatrixFromFile
  , distanceMatrix
    -- * Re-exports
  , BarCode(..)
  , Extended(..)
  )
where

import qualified System.Process.Typed          as SP
import qualified UnliftIO.Temporary            as U
import qualified System.IO                     as S
import qualified System.IO.Error               as S
import qualified Control.Exception             as X
import qualified Numeric.LinearAlgebra         as LA
import qualified Control.Foldl                 as FL
import qualified Data.Interval                 as I
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe
                                                , maybe
                                                , catMaybes
                                                )
import           Data.Void                      ( Void )
import qualified Data.Scientific               as Sci
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
import qualified Data.Text.IO                  as T
import qualified Data.Vector.Storable          as VS
import qualified Data.ByteString.Lazy          as BL
import           Control.Concurrent.STM         ( atomically )

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Byte          as P
import qualified Text.Megaparsec.Byte.Lexer    as PL
import           Data.Word                      ( Word8 )

import qualified Persistence.Filtration        as PH
import           Persistence.Filtration         ( BarCode(..)
                                                , Extended(..)
                                                )
import qualified Control.Applicative           as A
import           Control.Applicative            ( (<|>) )

import           Math.NumberTheory.Powers.Squares
                                                ( exactSquareRoot )

data Input = LowerDistance (LA.Matrix Double) | PointCloud (LA.Matrix Double) | Sparse [(Int, Int, Double)] deriving (Show)
type Dimension = Int -- specify max dimension for computed persistent homology
type Threshold = Double -- only compute Rips complexes up to this diameter
type Ratio = Double -- only report persistence pairs with death/birth > Ratio

type Distance a = (a -> a -> Double) -- should be >= 0 for all a

data PersistenceInterval = PersistenceInterval { dim :: Int, interval :: PH.BarCode Double } deriving (Show)

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
      dimOpt    = optFromMaybe "--dim " dimM
      threshOpt = optFromMaybe "--threshold " threshM
      ratioOpt  = optFromMaybe "--ratio " ratioM
      inputOpt  = "--format " <> case input of
        LowerDistance _ -> "lower-distance"
        PointCloud    _ -> "point-cloud"
        Sparse        _ -> "sparse"
      opts = [dimOpt, threshOpt, ratioOpt, inputOpt]
      cmd  = ripserPath ++ " " ++ T.unpack (T.intercalate " " opts)
--  putStrLn $ "input: " ++ show input
  let encodedText = encodeInput input
--  putStrLn $ "Encoded input: " ++ T.unpack encodedText
  let encodedBS = TL.encodeUtf8 $ TL.fromStrict $ encodeInput input
  unparsed <- U.withSystemTempFile "ripserIn" $ \fpIn hIn -> do
    U.withSystemTempFile "ripserOut" $ \fpOut hOut -> do
      BL.hPutStr hIn encodedBS
      S.hClose hIn
      S.withBinaryFile fpIn S.ReadMode $ \h' -> do
        let ripserProc =
              SP.setStdin (SP.useHandleClose h')
                $ SP.setStdout (SP.useHandleClose hOut)
                $ SP.shell cmd --(ripserPath <> " --format lower-distance")
        SP.runProcess_ ripserProc
        BL.readFile fpOut
--  putStr $ "UnParsed output:\n" ++ show unparsed
--  return []
  case parseOutput unparsed of
    Left parseErr ->
      X.throwIO
        $  S.userError
        $  "parse failure on output of ripser: "
        ++ (T.unpack parseErr)
    Right x -> return x

encodeInput :: Input -> T.Text
encodeInput (LowerDistance mLD) =
  let (rows, cols) = LA.size mLD
      nums         = T.intercalate " " $ fmap (T.pack . show) $ do
        r <- [0 .. (rows - 1)]
        c <- [0 .. (r - 1)]
        return $ mLD `LA.atIndex` (r, c)
  in  nums
encodeInput (PointCloud mPC) =
  let rowToCSV = T.intercalate "," . fmap (T.pack . show) . VS.toList
  in  T.intercalate "\n" $ fmap rowToCSV $ LA.toRows mPC
encodeInput (Sparse lSP) = T.intercalate "\n" $ fmap
  (\(i, j, d) ->
    (T.pack $ show i) <> " " <> (T.pack $ show j) <> " " <> (T.pack $ show d)
  )
  lSP

type Parser a = P.Parsec Void BL.ByteString a

void :: Functor f => f a -> f ()
void = fmap (const ())

space = PL.space P.space1 A.empty A.empty

lexeme = PL.lexeme space

trimLeading :: Parser ()
trimLeading = PL.space P.space1 A.empty A.empty -- spaces

parseOutput :: BL.ByteString -> Either T.Text [PersistenceInterval]
parseOutput ro =
  either (Left . T.pack . P.errorBundlePretty) Right
    $ P.runParser p "ripser-output" ro
 where
  parseDimLine :: Parser Int
  parseDimLine = do
    void $ P.string "persistence intervals in dim "
    dim <- lexeme PL.decimal
    void $ P.string ":"
    P.eol
    return dim
  parseBarCode :: Parser (PH.BarCode Double)
  parseBarCode = do
    let pNumber =
          fmap Sci.toRealFloat $ lexeme $ PL.signed (return ()) PL.scientific
        pExtended = (PH.Finite <$> pNumber) <|> (PH.Infinity <$ P.space1)
    void $ P.string "["
    birth <- pNumber
    void $ P.string ","
    death <- pExtended
    void $ P.string ")"
    return $ (birth, death)
  parseBarCodes :: Parser [PH.BarCode Double]
  parseBarCodes = A.some parseOne
   where
    parseOne = do
      trimLeading
      bc <- parseBarCode
      P.eol
      return bc
  parseDim :: Parser [PersistenceInterval]
  parseDim = do
    d        <- parseDimLine
    barCodes <- parseBarCodes
    return $ fmap (PersistenceInterval d) barCodes
  p :: Parser [PersistenceInterval]
  p = do
    dim0 <- P.skipManyTill
      (P.takeP (Just "skipping chars en route to first dim line") 1)
      parseDim --ignoreLinesUntil parseDimLine
    otherDims <- concat <$> A.many parseDim
    return $ dim0 ++ otherDims

toWord8 :: Char -> Word8
toWord8 = toEnum . fromEnum

nonNumber :: Parser ()
nonNumber = PL.space
  (void $ A.some $ P.noneOf
    ( fmap toWord8
    $ ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', '.', 'e', 'E']
    )
  )
  A.empty
  A.empty

numbers :: Parser [Double]
numbers = fmap (fmap Sci.toRealFloat) $ (trimLeading <|> void P.eol) >> A.many
  (PL.lexeme nonNumber (PL.signed (return ()) PL.scientific))

lowerDistanceMatrixFromFile :: FilePath -> IO Input
lowerDistanceMatrixFromFile fp = do
  asBS <- BL.readFile fp
  nums <- case P.runParser numbers fp asBS of
    Left errB -> X.throwIO $ S.userError
      (  "parse failure on lower distance matrix file: "
      ++ P.errorBundlePretty errB
      )
    Right x -> return x
  let n = L.length nums
  putStrLn $ "lowerDistanceMatrixFromFile: Loaded " ++ show n ++ " numbers"
  nCols <- case exactSquareRoot ((8 * n) + 1) of
    Nothing -> X.throwIO $ S.userError
      ("Loaded " ++ show n ++ " numbers from file but that is not triangular!")
    Just r -> return $ ((r - 1) `div` 2) + 1 -- +1 here because we have only below diagonal
  putStrLn $ "lowerDistanceMatrixFromFile: LDM has " ++ show nCols ++ " columns"
  let indices = do
        row <- [0 .. (nCols - 1)]
        col <- [0 .. (row - 1)]
        return (row, col)
      mLD = LA.assoc (nCols, nCols) 0 $ zip indices nums
  return $ LowerDistance mLD

-- untested!!
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

-- untested !!
sparseDistanceMatrix
  :: Foldable f
  => Distance a -- ^ distance function
  -> Double -- ^ threshold for removing from sparse representation
  -> f a -- ^ items
  -> Input
sparseDistanceMatrix d thresh items = Sparse (concat $ go itemsL)
 where
  itemsL = FL.fold FL.list items
  n      = L.length items
  oneItem r c x y =
    let h = d x y in if h >= thresh then Just (r, c, h) else Nothing
  oneRow [] = []
  oneRow (x : xs) =
    let c = n - 1 - L.length xs
    in  catMaybes $ fmap (\(r, y) -> oneItem r c x y) $ zip [(c + 1) ..] xs
  go []       = []
  go (x : xs) = oneRow (x : xs) : go xs



