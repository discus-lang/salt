{-# LANGUAGE TemplateHaskell #-}

module Waves.Prop.Core.Exp.Codec where

import qualified Waves.Gen.Core.Exp as GenX
import qualified Waves.Prop.Core.Codec as Codec

import qualified Salt.Core.Codec.Text.Parser.Term as Parser
import qualified Salt.Core.Codec.Text.Parser.Type as Parser

import           Hedgehog
import qualified Hedgehog.Range as Range


prop_type_roundtrip_parse :: Property
prop_type_roundtrip_parse = property $ do
  x <- forAll GenX.type_
  tripping x Codec.textOfData (Codec.dataOfText Parser.pType)

prop_value_roundtrip_parse :: Property
prop_value_roundtrip_parse = property $ do
  x <- forAll GenX.valuePrimitive
  tripping x Codec.textOfData (Codec.dataOfText Parser.pValue)

prop_string_roundtrip :: Property
prop_string_roundtrip = property $ do
  x <- forAll $ GenX.valueText $ Range.linear 0 1000
  -- NOTE: This needs the latest version of inchworm on github.
  -- NOTE: You may need to upgrade if this fails.
  tripping x Codec.textOfData (Codec.dataOfText Parser.pValue)

tests :: IO Bool
tests = checkParallel $$(discover)
