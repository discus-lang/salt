
module Waves.Gen.Core.Exp where

import           Salt.Core.Exp
-- import           Salt.Core.Exp.Type

import qualified Waves.Gen.Corpus as Corpus

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

name :: Gen Name
name = Gen.choice
  [ mk <$> Gen.element Corpus.colours <*> Gen.element Corpus.fruits
  -- , Name <$> Gen.text (Range.linear 0 100) Gen.unicodeAll
  ]
 where
  mk x y = Name (x <> "_" <> y)

bind :: Gen Bind
bind = Gen.choice
  [ BindName <$> name
  , return BindNone ]

bound :: Gen Bound
bound = Bound <$> name

type_ :: Gen (Type ())
type_ = Gen.recursive Gen.choice
  [ TVar <$> bound
  , TRef <$> typeRef
  ]
  [ TAbs <$> typeParams <*> type_
  , TKey <$> typeKey <*> Gen.list (Range.linear 0 3) typeArgs
  ]

typeRef :: Gen TypeRef
typeRef = Gen.choice
  [ TRPrm <$> name
  , TRCon <$> name
  ]

typeParams :: Gen (TypeParams ())
typeParams = TPTypes <$> Gen.list (Range.linear 0 3) ((,) <$> bind <*> type_)

typeArgs :: Gen (TypeArgs ())
typeArgs = TGTypes <$> Gen.list (Range.linear 0 3) type_

typeKey :: Gen TypeKey
typeKey = Gen.choice
  [ return TKHole
  , return TKArr
  , return TKApp
  , return TKFun
  , return TKForall
  , return TKExists
  , TKRecord <$> Gen.list (Range.linear 0 3) name
  , TKVariant <$> Gen.list (Range.linear 0 3) name
  ]


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

