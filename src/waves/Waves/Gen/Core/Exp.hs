
module Waves.Gen.Core.Exp where

import           Salt.Core.Exp
-- import           Salt.Core.Exp.Type

import qualified Waves.Gen.Corpus as Corpus

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as Text

-- | Usually generate using 'normal' names, but sometimes do weird names.
-- We want to shrink to normal names to ensure we have nice counterexamples.
nameFlip :: Gen Text -> Gen Name
nameFlip normal = Gen.frequency
  [ (9, Name <$> normal)
  , (1, Name <$> Gen.text (Range.linear 0 100) Gen.unicodeAll)
  ]

nameVar :: Gen Name
nameVar = nameFlip $ Gen.element Corpus.colours

namePrim :: Gen Name
namePrim = nameFlip $ Gen.element Corpus.fruits

nameCon :: Gen Name
nameCon = nameFlip (Text.toUpper <$> Gen.element Corpus.fruits)

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

  -- Don't generate annotations. Function stripAnnot replaces annotation contents:
  -- > (TAnn x e ~> TAnn () e),
  -- but in this case it'd be useful to rewrite
  -- > (TAnn x e ~> e)

  -- , TAnn () <$> type_
  ]

typeRef :: Gen (TypeRef ())
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
  , (,) TKForall <$> tAbs
  , (,) TKExists <$> tAbs
  , (\nts -> (TKRecord (map fst nts), map (TGTypes . (:[]) . snd) nts)) <$> typeFields
  , (\nts -> (TKVariant (map fst nts), map (TGTypes . (:[]) . snd) nts)) <$> typeFields
  -- TODO: generate types for the following keys
  -- , argsN (return TKSusp) [r03, r11]
  -- , argsN (return TKPure) []
  -- , argsN (return TKSync) []
  -- , argsN (return TKSum)  [r03]
  ]
 where
  argsN k ns = (,) <$> k <*> mapM (\r -> TGTypes <$> Gen.list r type_) ns
  r11 = Range.linear 1 1
  r13 = Range.linear 1 3
  r03 = Range.linear 0 3
  tAbs = ((:[]) . TGTypes . (:[])) <$> (TAbs <$> typeParams <*> type_)

typeFields :: Gen [(Name, Type ())]
typeFields = Gen.list (Range.linear 0 3) ((,) <$> nameVar <*> type_)



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
  , valueText (Range.linear 0 100)
  ]

valueText :: Range Int -> Gen (Value ())
valueText len = VText <$> Gen.choice
  -- Restricting the generator to ascii characters is more likely to find escaping issues
  [ Gen.text len Gen.ascii
  , Gen.text len Gen.unicodeAll
  ]
