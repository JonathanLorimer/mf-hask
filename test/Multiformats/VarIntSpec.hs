{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Multiformats.VarintSpec where

import           Data.Either
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List as L
import           Text.Megaparsec
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Multiformats.Varint
import           Relude

spec :: Spec
spec = do

  describe "takeByte" $ do
    let tooShort    = "0000"
    let tooLong     = "0101010101010101010101"
    let goldilocks  = "11111111"

    it "should parse an 8 digit bytestring" $ do
      parse takeByte "" goldilocks
        `shouldBe` Right goldilocks
    it "should parse long bytestring to 8 digit chunk" $ do
      parse takeByte "" tooLong
        `shouldBe` Right "01010101"
    it "should fail on input that is < 8 characters" $ do
      parse takeByte "" tooShort
        `shouldSatisfy` isLeft

  describe "parseVarByte" $ do

    it "should parse a varint without a continuation" $ do
      parse parseVarByte "" "01001111"
        `shouldBe`
          (Right $ VarintByte Zero
            [ One, One, One, One, Zero, Zero, One])

    it "should parse a varint with a continuation" $ do
      parse parseVarByte "" "11001111"
        `shouldBe`
          (Right $ VarintByte One
            [One, One, One, One, Zero, Zero, One])

    it "should fail on non-binary input in continuation" $ do
      parse parseVarByte "" "31001111"
        `shouldSatisfy` isLeft

    it "should fail on non-binary input in word 7" $ do
      parse parseVarByte "" "01601811"
        `shouldSatisfy` isLeft

  describe "parseRawBinary" $ do
    it "should fail on non-binary input in cont" $ do
      parse parseRawBinary "" "70000000"
        `shouldSatisfy` isLeft

    it "should fail on non-binary input in w7" $ do
      parse parseRawBinary "" "00090000"
        `shouldSatisfy` isLeft

    it "should parse an 8 digit bytestring" $ do
      parse parseRawBinary "" "01111111"
        `shouldBe` Right [One,One,One,One,One,One,One]

    it "should fail on 8 digit bytestring with continuation" $ do
      parse parseRawBinary "" "11111111"
        `shouldSatisfy` isLeft

    it "should parse a 16 digit bytestring" $ do
      let testBin =  "10000001"
                  <> "00010101"
      parse parseRawBinary "" testBin
        `shouldBe` (Right $  [One, Zero, Zero, Zero, Zero, Zero, Zero]
                         <> [One ,Zero, One, Zero, One, Zero, Zero])

    it "should parse a 8 digits from 16 digit bytestring without continuation" $ do
      let testBin =  "00000001"
                  <> "00010101"
      parse parseRawBinary "" testBin
        `shouldBe` Right [One, Zero, Zero, Zero, Zero, Zero, Zero]

  describe "binToDec" $ do

    it "should convert basic binary numbers to decimal" $ do
      binToDec [Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero]
        `shouldBe` (0 :: Int)
      binToDec [One, Zero, Zero, Zero, Zero, Zero, Zero, Zero]
        `shouldBe` (1 :: Int)
      binToDec [Zero, One, Zero, Zero, Zero, Zero, Zero, Zero]
        `shouldBe` (2 :: Int)
      binToDec [One, One, Zero, Zero, Zero, Zero, Zero, Zero]
        `shouldBe` (3 :: Int)
      binToDec [Zero, Zero, One, Zero, Zero, Zero, Zero, Zero]
        `shouldBe` (4 :: Int)
      binToDec [One, Zero, One, Zero, Zero, Zero, Zero, Zero]
        `shouldBe` (5 :: Int)
      binToDec [Zero, One, One, Zero, Zero, Zero, Zero, Zero]
        `shouldBe` (6 :: Int)
      binToDec [One, One, One, Zero, Zero, Zero, Zero, Zero]
        `shouldBe` (7 :: Int)
      binToDec [Zero, Zero, Zero, One, Zero, Zero, Zero, Zero]
        `shouldBe` (8 :: Int)
      binToDec [One, Zero, Zero, One, Zero, Zero, Zero, Zero]
        `shouldBe` (9 :: Int)
      binToDec [Zero, One, Zero, One, Zero, Zero, Zero, Zero]
        `shouldBe` (10 :: Int)
      binToDec [Zero, Zero, Zero, Zero, One, Zero, Zero, Zero]
        `shouldBe` (16 :: Int)
      binToDec [Zero, Zero, Zero, Zero, Zero, One, Zero, Zero]
        `shouldBe` (32 :: Int)
      binToDec [Zero, Zero, Zero, Zero, Zero, Zero, One, Zero]
        `shouldBe` (64 :: Int)
      binToDec [Zero, Zero, Zero, Zero, Zero, Zero, Zero, One]
        `shouldBe` (128 :: Int)
      binToDec [One, One, One, One, One, One, One, One]
        `shouldBe` (255 :: Int)

  describe "parseVarint" $ do

    it "should convert basic bit strings to a varint" $ do
      parse parseVarint "" "00000001"
        `shouldBe` (Right $ Varint (1 :: Int))
      parse parseVarint "" "01111111"
        `shouldBe` (Right $ Varint (127 :: Int))
      parse parseVarint "" ("10000000" <> "00000001")
        `shouldBe` (Right $ Varint (128 :: Int))
      parse parseVarint "" ("11111111" <> "00000001")
        `shouldBe` (Right $ Varint (255 :: Int))
      parse parseVarint "" ("10101100" <> "00000010")
        `shouldBe` (Right $ Varint (300 :: Int))
      parse parseVarint "" ("10000000" <> "10000000" <> "00000001")
        `shouldBe` (Right $ Varint (16384 :: Int))

  describe "decToBin" $ do

    it "should convert basic integrals to a binary representation" $ do
      decToBin (0 :: Int)
        `shouldBe` [Zero]
      decToBin (1 :: Int)
        `shouldBe` [One]
      decToBin (2 :: Int)
        `shouldBe` [Zero, One]
      decToBin (3 :: Int)
        `shouldBe` [One, One]
      decToBin (4 :: Int)
        `shouldBe` [Zero, Zero, One]
      decToBin (5 :: Int)
        `shouldBe` [One, Zero, One]
      decToBin (6 :: Int)
        `shouldBe` [Zero, One, One]
      decToBin (7 :: Int)
        `shouldBe` [One, One, One]
      decToBin (8 :: Int)
        `shouldBe` [Zero, Zero, Zero, One]
      decToBin (9 :: Int)
        `shouldBe` [One, Zero, Zero, One]
      decToBin (10 :: Int)
        `shouldBe` [Zero, One, Zero, One]
      decToBin (16 :: Int)
        `shouldBe` [Zero, Zero, Zero, Zero, One]
      decToBin (32 :: Int)
        `shouldBe` [Zero, Zero, Zero, Zero, Zero, One]
      decToBin (64 :: Int)
        `shouldBe` [Zero, Zero, Zero, Zero, Zero, Zero, One]
      decToBin (128 :: Int)
        `shouldBe` [Zero, Zero, Zero, Zero, Zero, Zero, Zero, One]
      decToBin (255 :: Int)
        `shouldBe` [One, One, One, One, One, One, One, One]

-- TODO: add tests for padBit

  describe "padByte" $ do

    it "should leave a byte alone" $ do
      let byte = [Zero, One, Zero, One, Zero, One, Zero, One]
      padByte byte `shouldBe` byte

    it "should complete 5 bits with 3 zeros" $ do
      let bits = [Zero, One, Zero, One, Zero]
      padByte bits `shouldBe` [Zero, One, Zero, One, Zero, Zero, Zero, Zero]

    it "should complete 9 bits with 7 zeros" $ do
      let bits = [Zero, One, Zero, One, Zero, One, One, One, One]
      let seven = [Zero, Zero, Zero, Zero, Zero, Zero, Zero]
      padByte bits `shouldBe` (bits <> seven)

  describe "Properties for encode and decode" $ do

    prop "encode . decode == id"
      $ forAllShrink (choose (0, 10^512)) shrinkIntegral encodeDecode
    prop "decode . encode == id"
      $ forAllShrink (listOf (arbitrary :: Gen Binary)) genericShrink decodeEncode

encodeDecode :: Integer -> Bool
encodeDecode i = Just (Varint i) == (decode . encode $ Varint i)

decodeEncode :: [Binary] -> Bool
decodeEncode bins = compBs p == (encode <$> (decode bs))
  where
    p = padByte bins
    bs = B.pack $ binToW8 <$> p

compBs :: [Binary] -> Maybe B.ByteString
compBs = (fmap $ B.pack . (fmap binToW8))
       . checkConts
       . (break $ \x -> NE.head x == Zero)
       . (fmap NE.fromList)
       . (groupsOf 8)

testBS :: [Binary] -> ([NonEmpty Binary], [NonEmpty Binary])
testBS =  (break $ \x -> NE.head x == Zero)
       . (fmap NE.fromList)
       . (groupsOf 8)

checkConts :: ([NonEmpty Binary], [NonEmpty Binary]) -> Maybe ([Binary])
checkConts (_ , []) = Nothing
checkConts ([], b) = NE.toList <$> (viaNonEmpty head b)
checkConts (f, s) =
  case viaNonEmpty head s of
    Nothing -> Just $ join $ NE.toList <$> f
    Just s' -> if all (== Zero) (NE.toList s')
                  then Just $
                    if length f > 1
                       then join
                        $  (NE.toList <$> (L.init f))
                        <> [Zero : (NE.drop 1 $ L.last f)]
                       else Zero : (NE.drop 1 $ L.head f)
                  else Just $ NE.toList $ (join $ fromList f) <> (s')
