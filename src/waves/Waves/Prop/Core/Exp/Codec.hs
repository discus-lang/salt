{-# LANGUAGE TemplateHaskell #-}

module Waves.Prop.Core.Exp.Codec where

import qualified Waves.Gen.Core.Exp as GenX
import qualified Waves.Prop.Core.Codec as Codec

import qualified Salt.Core.Codec.Text.Parser.Term as Parser
import qualified Salt.Core.Codec.Text.Parser.Type as Parser

import           Hedgehog


prop_type_roundtrip_parse_plain :: Property
prop_type_roundtrip_parse_plain = property $ do
  x <- forAll GenX.type_
  tripping x Codec.textOfDataPlain (Codec.dataOfText Parser.pType)

prop_type_roundtrip_parse_indent :: Property
prop_type_roundtrip_parse_indent = property $ do
  x <- forAll GenX.type_
  tripping x Codec.textOfDataIndent (Codec.dataOfText Parser.pType)

prop_value_roundtrip_parse_plain :: Property
prop_value_roundtrip_parse_plain = property $ do
  x <- forAll GenX.valuePrimitive
  tripping x Codec.textOfDataPlain (Codec.dataOfText Parser.pValue)

prop_value_roundtrip_parse_indent :: Property
prop_value_roundtrip_parse_indent = property $ do
  x <- forAll GenX.valuePrimitive
  tripping x Codec.textOfDataIndent (Codec.dataOfText Parser.pValue)

tests :: IO Bool
tests = checkParallel $$(discover)
