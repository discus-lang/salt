{-# LANGUAGE TemplateHaskell #-}
module Waves.Prop.Core.Exp.Term where

import           Salt.Core.Exp.Term
import qualified Salt.Core.Codec.Text.Lexer       as Lexer
import qualified Salt.Core.Codec.Text.Parser      as Parser
import qualified Salt.Core.Codec.Text.Parser.Term as Parser.Term
import           Salt.Core.Codec.Text.Pretty ()
import qualified Salt.Core.Transform.MapAnnot as MapAnnot

import qualified Salt.Data.Pretty as Pretty

import qualified Text.Lexer.Inchworm.Char       as IW

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import System.IO.Unsafe (unsafePerformIO)

-- Something bigger than a machine int
largeInteger :: Integer
largeInteger = 2^(100 :: Integer)

genSimpleValue :: Gen (Value ())
genSimpleValue
 = Gen.choice
        [ return VUnit
        , VBool <$> Gen.bool
        , VNat <$> Gen.integral (Range.linear 0 largeInteger)
        -- TODO: the pretty-printer is ambiguous for positive ints and nats
        , VInt <$> Gen.integral (Range.linear (-largeInteger) (-1))
        , VText <$> Gen.text (Range.linear 0 100) Gen.unicodeAll
        ]

textOfValue :: Value a -> String
textOfValue = Pretty.renderPlain . Pretty.ppr ()

data RoundtripError
  = ErrorParse String String
  | ErrorLexLeftover String String
  | ErrorParseLeftover String (Value ()) String
  deriving (Eq, Show)

valueOfText :: String -> Either RoundtripError (Value ())
valueOfText text =
 let fp = "<test>"
     -- Is there a pure lexer?
     (toks,_,strRest) = unsafePerformIO $ IW.scanStringIO text (Lexer.scanner fp)
  in case strRest of
      [] -> case Parser.parse Parser.Term.pValue fp toks of
             Left p -> Left $ ErrorParse text (show p)
             Right (v, []) -> return $ MapAnnot.stripAnnot v
             Right (v,tokRest) -> Left $ ErrorParseLeftover text (MapAnnot.stripAnnot v) (show tokRest)
      _  -> Left $ ErrorLexLeftover text strRest

prop_value_roundtrip_parse :: Property
prop_value_roundtrip_parse = property $ do
  x <- forAll genSimpleValue
  tripping x textOfValue valueOfText

tests :: IO Bool
tests = checkParallel $$(discover)
