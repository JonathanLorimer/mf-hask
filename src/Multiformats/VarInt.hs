{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Multiformats.Varint where

import qualified Data.ByteString as B
import qualified Data.List as L
import Text.Megaparsec
import Text.Megaparsec.Byte (binDigitChar)
import Test.QuickCheck
import Relude hiding (many)

-- | Parser Types
type Parser = Parsec BinaryError B.ByteString
data BinaryError = BinaryError B.ByteString
  deriving (Ord, Eq, Show)
type VarintParseError = ParseErrorBundle ByteString BinaryError

-- | Binary Representations
data Binary = Zero | One deriving (Ord, Eq, Show)
instance Arbitrary Binary where
  arbitrary = do
    n <- (arbitrary :: Gen Bool)
    pure $ if n then One else Zero

data VarintByte = VarintByte
                { continuation :: Binary
                , word7 :: [Binary]
                } deriving (Ord, Eq, Show)
newtype Varint a = Varint a
  deriving (Eq, Ord, Show, Read, Num)

-- | Position Parsers
takeByte :: Parser B.ByteString
takeByte = takeP (Just "Varint Byte") 8


-- | Parsers
parseRawBinary :: Parser [Binary]
parseRawBinary = do
  byte <- takeByte
  case parseMaybe parseVarByte byte of
    Nothing    -> customFailure
               $  BinaryError
               $  "invalid byte: " <> byte
    Just (VarintByte Zero w7) -> pure w7
    Just (VarintByte One w7)  -> (w7 <>) <$> parseRawBinary


parseVarByte :: Parser VarintByte
parseVarByte = do
  cont <- parseBit
  w7   <- count 7 parseBit
  pure $ VarintByte cont (reverse w7)

parseVarint :: Num a => Parser (Varint a)
parseVarint = binToDec <$> parseRawBinary

parseBit :: Parser Binary
parseBit = do
  bit <- binDigitChar
  case bit of
    48 -> pure Zero
    49 -> pure One
    x  -> customFailure
       $  BinaryError
       $  "invalid bit: " <>  (B.pack [x])

-- | User Functions
decode :: Num a => B.ByteString -> Maybe (Varint a)
decode bs =
  case parse parseVarint "Multiformats.Varint.hs" bs of
    Left  _   -> Nothing
    Right vi  -> Just vi

decodeEither :: Num a
             => B.ByteString
             -> Either VarintParseError (Varint a)
decodeEither = parse parseVarint "Multiformats.Varint.hs"

encode :: Integral a => Varint a -> B.ByteString
encode (Varint n) =
    B.pack
  . (fmap binToW8)
  . (uncurry (<>))
  . bimap
      (foldr
        (\x acc -> (One : (L.reverse x)) <> acc)
        [])
      (maybe [] $ (Zero :) . L.reverse . (padBits 7))
  . initLast
  . (groupsOf 7)
  . decToBin
  $ n

-- | Helpers
binBool :: Binary -> Bool
binBool Zero = False
binBool One = True

binToW8 :: Binary -> Word8
binToW8 Zero = 48
binToW8 One = 49

padBits :: Int -> [Binary] -> [Binary]
padBits n xs = xs <> padding
  where
    l = length xs
    padding
      | l `mod` n == 0 = mempty
      | l < n          = replicate (n - l) Zero
      | otherwise      = replicate (n - (l `mod` n)) Zero

padByte :: [Binary] -> [Binary]
padByte = padBits 8

binToDec :: Num a => [Binary] -> a
binToDec = snd . foldl' go (0, 0)
  where
    go :: Num a => (Int, a) -> Binary -> (Int, a)
    go (e, val) Zero = (e + 1, val)
    go (e, val) One = (e + 1, val + (2^e))


decToBin :: Integral a => a -> [Binary]
decToBin 0   = [Zero]
decToBin num = go num []
  where
    go 0 xs = xs
    go n xs = case n `divMod` 2 of
                (d, 1) -> One : (go d xs)
                (d, 0) -> Zero : (go d xs)
                (d, _) -> One : (go d xs)
                -- | TODO: Add error case here, perhaps some type level stuff

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : (groupsOf n (drop n xs))

initLast :: [a] -> ([a], Maybe a)
initLast xs = fmap go $ splitAt ((length xs) - 1) xs
  where
    go []       = Nothing
    go (a : _) = Just a
    -- | TODO: Add error case here, perhaps some type level stuff





