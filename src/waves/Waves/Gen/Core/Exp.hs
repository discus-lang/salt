
module Waves.Gen.Core.Exp where

import           Salt.Core.Exp
-- import           Salt.Core.Exp.Type

import qualified Waves.Gen.Corpus as Corpus

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as Text

nameVar :: Gen Name
nameVar = Gen.choice
  [ Name <$> Gen.element Corpus.colours
  -- TODO: generate more interesting names
  -- , Name <$> Gen.text (Range.linear 0 100) Gen.unicodeAll
  ]

namePrim :: Gen Name
namePrim = Name <$> (Gen.element Corpus.fruits)

nameCon :: Gen Name
nameCon = Name <$> (Text.toUpper <$> Gen.element Corpus.fruits)

bind :: Gen Bind
bind = Gen.choice
  [ BindName <$> nameVar
  , return BindNone ]

bound :: Gen Bound
bound = Bound <$> nameVar

type_ :: Gen (Type ())
type_ = Gen.recursive Gen.choice
  [ TVar <$> bound
  , TRef <$> typeRef
  ]
  [ TAbs <$> typeParams <*> type_
  , uncurry TKey <$> typeKey
  ]

typeRef :: Gen TypeRef
typeRef = Gen.choice
  [ TRPrm <$> namePrim
  , TRCon <$> nameCon
  ]

typeParams :: Gen (TypeParams ())
typeParams = TPTypes <$> Gen.list (Range.linear 1 3) ((,) <$> bind <*> type_)

typeArgs :: Gen (TypeArgs ())
typeArgs = TGTypes <$> Gen.list (Range.linear 1 3) type_

typeKey :: Gen (TypeKey, [TypeArgs ()])
typeKey = Gen.choice
  [ argsN (return TKHole) []
  , argsN (return TKArr) [r13, r11]
  , argsN (return TKApp) [r11, r13]
  , argsN (return TKFun) [r03, r03]
  -- TODO: generate types for the following keys
  -- , argsN (return TKForall) [r11]
  -- , argsN (return TKExists) [r11]
  -- , argsN (TKRecord <$> Gen.list (Range.linear 1 3) nameCon) [r03]
  -- , argsN (TKVariant <$> Gen.list (Range.linear 1 3) nameCon) [r03]
  ]
 where
  argsN k ns = (,) <$> k <*> mapM (\r -> TGTypes <$> Gen.list r type_) ns
  r11 = Range.linear 1 1
  r13 = Range.linear 1 3
  r03 = Range.linear 0 3


-- Something bigger than a machine int
largeInteger :: Integer
largeInteger = 2^(100 :: Integer)

valuePrimitive :: Gen (Value ())
valuePrimitive = Gen.choice
  [ return VUnit
  , VBool <$> Gen.bool
  , VNat <$> Gen.integral (Range.linear 0 largeInteger)
  -- TODO: the pretty-printer is ambiguous for positive ints and nats
  , VInt <$> Gen.integral (Range.linear (-largeInteger) (-1))
  , VText <$> Gen.text (Range.linear 0 100) Gen.unicodeAll
  ]

