
module Salt.Core.Prim.Ops where
import Salt.Core.Transform.MapAnnot
import Salt.Core.Exp
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Data.Text.IO           as Text
import Data.Map                         (Map)


---------------------------------------------------------------------------------------------------
-- | Holds information about a primitive operator.
--   We keep all the info about an operator in once place,
--   instead of spread out all over the compiler and external documentation.
data Prim
        -- Define a pure primitive operator.
        -- Provided the values match the expected types,
        -- these operators always succeed, and perform no actions.
        = PP
        { name  :: Name
        , tsig  :: Type ()
        , step  :: forall a. [TermNormals a] -> [Value a]
        , docs  :: Text }

        -- Define an operator that performs an action in the local process.
        | PO
        { name  :: Name
        , tsig  :: Type ()
        , teff  :: [Type ()]
        , exec  :: forall a. Show a => [TermNormals a] -> IO [Value a]
        , docs  :: Text }


-- | Get the value type of a primitive.
typeOfPrim :: Prim -> Type ()
typeOfPrim pp
 = case pp of
        PP {tsig} -> tsig
        PO {tsig} -> tsig


-- | Get the effect type of a primitive.
effectOfPrim :: Prim -> Type ()
effectOfPrim pp
 = case pp of
        PP {}     -> TPure
        PO {teff} -> TSum teff


primOps :: Map Name Prim
primOps
 = Map.fromList $ map (\p -> (name p, p)) $ concat
        [ primOpsBool,   primOpsNat
        , primOpsInt, primOpsInt8, primOpsInt16, primOpsInt32, primOpsInt64
        , primOpsWord, primOpsWord8, primOpsWord16, primOpsWord32, primOpsWord64
        , primOpsSymbol
        , primOpsList,   primOpsSet,  primOpsMap
        , primOpsDebug
        , primOpsConsole ]


-- Bool -------------------------------------------------------------------------------------------
primOpsBool
 = [ PP { name  = "bool'not"
        , tsig  = [TBool] :-> [TBool]
        , step  = \[NVs [VBool b]] -> [VBool (not b)]
        , docs  = "Boolean negation." }

   , PP { name  = "bool'and"
        , tsig  = [TBool, TBool] :-> [TBool]
        , step  = \[NVs [VBool b1, VBool b2]] -> [VBool $ b1 && b2]
        , docs  = "Boolean and." }

   , PP { name  = "bool'or"
        , tsig  = [TBool, TBool] :-> [TBool]
        , step  = \[NVs [VBool b1, VBool b2]] -> [VBool $ b1 || b2]
        , docs  = "Boolean or." }

  , PP { name  = "bool'eq"
        , tsig  = [TBool, TBool] :-> [TBool]
        , step  = \[NVs [VBool b1, VBool b2]] -> [VBool $ b1 == b2]
        , docs  = "Boolean or." }
   ]


-- Nat --------------------------------------------------------------------------------------------
primOpsNat
 = [ PP { name  = "nat'add"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 + n2]
        , docs  = "Natural number addition." }

   , PP { name  = "nat'sub"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 - n2]
        , docs  = "Natural number subtraction." }

   , PP { name  = "nat'mul"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 * n2]
        , docs  = "Natural number multiplication." }

   , PP { name  = "nat'div"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 `div` n2]
        , docs  = "Natural number division." }

   , PP { name  = "nat'rem"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 `rem` n2]
        , docs  = "Natural number remainder." }

   , PP { name  = "nat'eq"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 == n2]
        , docs  = "Natural number equality." }

   , PP { name  = "nat'neq"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 /= n2]
        , docs  = "Natural number negated equality." }

   , PP { name  = "nat'lt"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 < n2]
        , docs  = "Natural number less-than." }

   , PP { name  = "nat'le"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 <= n2]
        , docs  = "Natural number less-than or equal." }

   , PP { name  = "nat'gt"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 > n2]
        , docs  = "Natural number greater-than." }

   , PP { name  = "nat'ge"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 >= n2]
        , docs  = "Natural number greater-than or equal." }
   ]

-- Int --------------------------------------------------------------------------------------------
primOpsInt
 = [ PP { name  = "int"
        , tsig  = [TNat] :-> [TInt]
        , step  = \[NVs [VNat n]] -> [VInt $ n]
        , docs  = "Integer constructor." }

   , PP { name  = "int'add"
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int'sub"
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int'mul"
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int'div"
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int'rem"
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int'eq"
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int'neq"
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int'lt"
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int'le"
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int'gt"
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int'ge"
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]

-- Int8 --------------------------------------------------------------------------------------------
primOpsInt8
 = [ PP { name  = "int8"
        , tsig  = [TNat] :-> [TInt8]
        , step  = \[NVs [VNat n]] -> [VInt8 $ fromIntegral n]
        , docs  = "Integer constructor." }

   , PP { name  = "int8'add"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int8'sub"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int8'mul"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int8'div"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int8'rem"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int8'eq"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int8'neq"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int8'lt"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int8'le"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int8'gt"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int8'ge"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]

-- Int16 --------------------------------------------------------------------------------------------
primOpsInt16
 = [ PP { name  = "int16"
        , tsig  = [TNat] :-> [TInt16]
        , step  = \[NVs [VNat n]] -> [VInt16 $ fromIntegral n]
        , docs  = "Integer constructor." }

   , PP { name  = "int16'add"
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int16'sub"
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int16'mul"
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int16'div"
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int16'rem"
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int16'eq"
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int16'neq"
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int16'lt"
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int16'le"
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int16'gt"
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int16'ge"
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]

-- Int32 --------------------------------------------------------------------------------------------
primOpsInt32
 = [ PP { name  = "int32"
        , tsig  = [TNat] :-> [TInt32]
        , step  = \[NVs [VNat n]] -> [VInt32 $ fromIntegral n]
        , docs  = "Integer constructor." }

   , PP { name  = "int32'add"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int32'sub"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int32'mul"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int32'div"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int32'rem"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int32'eq"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int32'neq"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int32'lt"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int32'le"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int32'gt"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int32'ge"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]

-- Int64 --------------------------------------------------------------------------------------------
primOpsInt64
 = [ PP { name  = "int64"
        , tsig  = [TNat] :-> [TInt64]
        , step  = \[NVs [VNat n]] -> [VInt64 $ fromIntegral n]
        , docs  = "Integer constructor." }

   , PP { name  = "int64'add"
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int64'sub"
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int64'mul"
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int64'div"
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int64'rem"
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int64'eq"
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int64'neq"
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int64'lt"
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int64'le"
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int64'gt"
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int64'ge"
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]

-- Word --------------------------------------------------------------------------------------------
primOpsWord
 = [ PP { name  = "word"
        , tsig  = [TNat] :-> [TWord]
        , step  = \[NVs [VNat n]] -> [VWord $ fromIntegral n]
        , docs  = "Word constructor." }

   , PP { name  = "word'add"
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 + n2]
        , docs  = "Word addition." }

   , PP { name  = "word'sub"
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 - n2]
        , docs  = "Word subtraction." }

   , PP { name  = "word'mul"
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 * n2]
        , docs  = "Word multiplication." }

   , PP { name  = "word'div"
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 `div` n2]
        , docs  = "Word division." }

   , PP { name  = "word'rem"
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 `rem` n2]
        , docs  = "Word remainder." }

   , PP { name  = "word'eq"
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 == n2]
        , docs  = "Word equality." }

   , PP { name  = "word'neq"
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 /= n2]
        , docs  = "Word negated equality." }

   , PP { name  = "word'lt"
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 < n2]
        , docs  = "Word less-than." }

   , PP { name  = "word'le"
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 <= n2]
        , docs  = "Word less-than or equal." }

   , PP { name  = "word'gt"
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 > n2]
        , docs  = "Word greater-than." }

   , PP { name  = "word'ge"
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 >= n2]
        , docs  = "Word greater-than or equal." }
   ]

-- Word8 --------------------------------------------------------------------------------------------
primOpsWord8
 = [ PP { name  = "word8"
        , tsig  = [TNat] :-> [TWord8]
        , step  = \[NVs [VNat n]] -> [VWord8 $ fromIntegral n]
        , docs  = "Word constructor." }

   , PP { name  = "word8'add"
        , tsig  = [TWord8, TWord8] :-> [TWord8]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VWord8 $ n1 + n2]
        , docs  = "Word addition." }

   , PP { name  = "word8'sub"
        , tsig  = [TWord8, TWord8] :-> [TWord8]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VWord8 $ n1 - n2]
        , docs  = "Word subtraction." }

   , PP { name  = "word8'mul"
        , tsig  = [TWord8, TWord8] :-> [TWord8]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VWord8 $ n1 * n2]
        , docs  = "Word multiplication." }

   , PP { name  = "word8'div"
        , tsig  = [TWord8, TWord8] :-> [TWord8]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VWord8 $ n1 `div` n2]
        , docs  = "Word division." }

   , PP { name  = "word8'rem"
        , tsig  = [TWord8, TWord8] :-> [TWord8]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VWord8 $ n1 `rem` n2]
        , docs  = "Word remainder." }

   , PP { name  = "word8'eq"
        , tsig  = [TWord8, TWord8] :-> [TBool]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VBool $ n1 == n2]
        , docs  = "Word equality." }

   , PP { name  = "word8'neq"
        , tsig  = [TWord8, TWord8] :-> [TBool]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Word negated equality." }

   , PP { name  = "word8'lt"
        , tsig  = [TWord8, TWord8] :-> [TBool]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VBool $ n1 < n2]
        , docs  = "Word less-than." }

   , PP { name  = "word8'le"
        , tsig  = [TWord8, TWord8] :-> [TBool]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Word less-than or equal." }

   , PP { name  = "word8'gt"
        , tsig  = [TWord8, TWord8] :-> [TBool]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VBool $ n1 > n2]
        , docs  = "Word greater-than." }

   , PP { name  = "word8'ge"
        , tsig  = [TWord8, TWord8] :-> [TBool]
        , step  = \[NVs [VWord8 n1, VWord8 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Word greater-than or equal." }
   ]

-- Word16 --------------------------------------------------------------------------------------------
primOpsWord16
 = [ PP { name  = "word16"
        , tsig  = [TNat] :-> [TWord16]
        , step  = \[NVs [VNat n]] -> [VWord16 $ fromIntegral n]
        , docs  = "Word constructor." }

   , PP { name  = "word16'add"
        , tsig  = [TWord16, TWord16] :-> [TWord16]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VWord16 $ n1 + n2]
        , docs  = "Word addition." }

   , PP { name  = "word16'sub"
        , tsig  = [TWord16, TWord16] :-> [TWord16]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VWord16 $ n1 - n2]
        , docs  = "Word subtraction." }

   , PP { name  = "word16'mul"
        , tsig  = [TWord16, TWord16] :-> [TWord16]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VWord16 $ n1 * n2]
        , docs  = "Word multiplication." }

   , PP { name  = "word16'div"
        , tsig  = [TWord16, TWord16] :-> [TWord16]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VWord16 $ n1 `div` n2]
        , docs  = "Word division." }

   , PP { name  = "word16'rem"
        , tsig  = [TWord16, TWord16] :-> [TWord16]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VWord16 $ n1 `rem` n2]
        , docs  = "Word remainder." }

   , PP { name  = "word16'eq"
        , tsig  = [TWord16, TWord16] :-> [TBool]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VBool $ n1 == n2]
        , docs  = "Word equality." }

   , PP { name  = "word16'neq"
        , tsig  = [TWord16, TWord16] :-> [TBool]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Word negated equality." }

   , PP { name  = "word16'lt"
        , tsig  = [TWord16, TWord16] :-> [TBool]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VBool $ n1 < n2]
        , docs  = "Word less-than." }

   , PP { name  = "word16'le"
        , tsig  = [TWord16, TWord16] :-> [TBool]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Word less-than or equal." }

   , PP { name  = "word16'gt"
        , tsig  = [TWord16, TWord16] :-> [TBool]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VBool $ n1 > n2]
        , docs  = "Word greater-than." }

   , PP { name  = "word16'ge"
        , tsig  = [TWord16, TWord16] :-> [TBool]
        , step  = \[NVs [VWord16 n1, VWord16 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Word greater-than or equal." }
   ]

-- Word32 --------------------------------------------------------------------------------------------
primOpsWord32
 = [ PP { name  = "word32"
        , tsig  = [TNat] :-> [TWord32]
        , step  = \[NVs [VNat n]] -> [VWord32 $ fromIntegral n]
        , docs  = "Word constructor." }

   , PP { name  = "word32'add"
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 + n2]
        , docs  = "Word addition." }

   , PP { name  = "word32'sub"
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 - n2]
        , docs  = "Word subtraction." }

   , PP { name  = "word32'mul"
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 * n2]
        , docs  = "Word multiplication." }

   , PP { name  = "word32'div"
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 `div` n2]
        , docs  = "Word division." }

   , PP { name  = "word32'rem"
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 `rem` n2]
        , docs  = "Word remainder." }

   , PP { name  = "word32'eq"
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 == n2]
        , docs  = "Word equality." }

   , PP { name  = "word32'neq"
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Word negated equality." }

   , PP { name  = "word32'lt"
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 < n2]
        , docs  = "Word less-than." }

   , PP { name  = "word32'le"
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Word less-than or equal." }

   , PP { name  = "word32'gt"
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 > n2]
        , docs  = "Word greater-than." }

   , PP { name  = "word32'ge"
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Word greater-than or equal." }
   ]

-- Word64 --------------------------------------------------------------------------------------------
primOpsWord64
 = [ PP { name  = "word64"
        , tsig  = [TNat] :-> [TWord64]
        , step  = \[NVs [VNat n]] -> [VWord64 $ fromIntegral n]
        , docs  = "Word constructor." }

   , PP { name  = "word64'add"
        , tsig  = [TWord64, TWord64] :-> [TWord64]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VWord64 $ n1 + n2]
        , docs  = "Word addition." }

   , PP { name  = "word64'sub"
        , tsig  = [TWord64, TWord64] :-> [TWord64]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VWord64 $ n1 - n2]
        , docs  = "Word subtraction." }

   , PP { name  = "word64'mul"
        , tsig  = [TWord64, TWord64] :-> [TWord64]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VWord64 $ n1 * n2]
        , docs  = "Word multiplication." }

   , PP { name  = "word64'div"
        , tsig  = [TWord64, TWord64] :-> [TWord64]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VWord64 $ n1 `div` n2]
        , docs  = "Word division." }

   , PP { name  = "word64'rem"
        , tsig  = [TWord64, TWord64] :-> [TWord64]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VWord64 $ n1 `rem` n2]
        , docs  = "Word remainder." }

   , PP { name  = "word64'eq"
        , tsig  = [TWord64, TWord64] :-> [TBool]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VBool $ n1 == n2]
        , docs  = "Word equality." }

   , PP { name  = "word64'neq"
        , tsig  = [TWord64, TWord64] :-> [TBool]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Word negated equality." }

   , PP { name  = "word64'lt"
        , tsig  = [TWord64, TWord64] :-> [TBool]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VBool $ n1 < n2]
        , docs  = "Word less-than." }

   , PP { name  = "word64'le"
        , tsig  = [TWord64, TWord64] :-> [TBool]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Word less-than or equal." }

   , PP { name  = "word64'gt"
        , tsig  = [TWord64, TWord64] :-> [TBool]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VBool $ n1 > n2]
        , docs  = "Word greater-than." }

   , PP { name  = "word64'ge"
        , tsig  = [TWord64, TWord64] :-> [TBool]
        , step  = \[NVs [VWord64 n1, VWord64 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Word greater-than or equal." }
   ]

-- Symbol -----------------------------------------------------------------------------------------
primOpsSymbol
 = [ PP { name  = "symbol'eq"
        , tsig  = [TSymbol, TSymbol] :-> [TBool]
        , step  = \[NVs [VSymbol s1, VSymbol s2]] -> [VBool $ s1 == s2]
        , docs  = "Symbol equality comparison." }
   ]


-- List -------------------------------------------------------------------------------------------
primOpsList
 = [ PP { name  = "list'empty"
        , tsig  = [("a", TData)] :*> TList "a"
        , step  = \[NTs [t], NVs []] -> [VList t []]
        , docs  = "Construct an empty list" }

   , PP { name  = "list'nil"
        , tsig  = [("a", TData)] :*> TList "a"
        , step  = \[NTs [t], NVs []] -> [VList t []]
        , docs  = "Construct an empty list (same as #list'empty)." }

   , PP { name  = "list'one"
        , tsig  = [("a", TData)] :*> (["a"] :-> [TList "a"])
        , step  = \[NTs [t], NVs [v]] -> [VList t [v]]
        , docs  = "Construct a singleton list." }

   , PP { name  = "list'cons"
        , tsig  = [("a", TData)] :*> (["a", TList "a"] :-> [TList "a"])
        , step  = \[NTs [t], NVs [v, VList _ vs]]
                        -> [VList t (v : vs)]
        , docs  = "Attach an element to the front of an existing list." }

   , PP { name  = "list'isEmpty"
        , tsig  = [("a", TData)] :*> ([TList "a"] :-> [TBool])
        , step  = \[NTs [_], NVs [VList _ vs]]
                        -> [VBool $ null vs]
        , docs  = "Check if the given list is empty." }

   , PP { name  = "list'size"
        , tsig  = [("a", TData)] :*> ([TList "a"] :-> [TNat])
        , step  = \[NTs [_], NVs [VList _ vs]]
                        -> [VNat $ fromIntegral $ length vs]
        , docs  = "Produce the size of the given list." }

   , PP { name  = "list'head"
        , tsig  = [("a", TData)] :*> ([TList "a"] :-> [TOption "a"])
        , step  = \[NTs [t], NVs [VList _ vs]]
                        -> case vs of { [] -> [VNone t]; v : _ -> [VSome t v] }
        , docs  = "Take the head element of a list." }

   , PP { name  = "list'tail"
        , tsig  = [("a", TData)] :*> ([TList "a"] :-> [TOption (TList "a")])
        , step  = \[NTs [t], NVs [VList _ vs]]
                        -> case vs of { [] -> [VNone t]; v : _ -> [VSome t v] }
        , docs  = "Take the head element of a list." }

   , PP { name  = "list'case"
        , tsig  =  [("a", TData)] :*> ([TList "a"] :-> [makeListType "a"])
        , step  = \[NTs [t], NVs [VList _ vv]]
                  -> case vv of
                      []      -> [VVariant "nil"  (makeListType t) []]
                      v : vs  -> [VVariant "cons" (makeListType t) [v, VList t vs]]
        , docs  = "Case analysis on a list." }
   ]

makeListType :: Type a -> Type a
makeListType tArg
 = TVariant
        ["nil", "cons"]
        [ TGTypes [], TGTypes [tArg, TList tArg] ]


-- Set --------------------------------------------------------------------------------------------
primOpsSet
 = [ PP { name  = "set'empty"
        , tsig  = [("a", TData)] :*> ([] :-> [TSet "a"])
        , step  = \[NTs [t], NVs []]
                        -> [VSet t $ Set.empty]
        , docs  = "Construct an empty set." }

   , PP { name  = "set'fromList"
        , tsig  = [("a", TData)] :*> (["a"] :-> [TList "a"])
        , step  = \[NTs [t], NVs [VList _ vs]]
                        -> [VSet t $ Set.fromList $ map stripAnnot vs]
        , docs  = "Construct a set from a list of values." }

   , PP { name  = "set'isEmpty"
        , tsig  = [("a", TData)] :*> ([TSet "a"] :-> [TBool])
        , step  = \[NTs [_], NVs [VSet _ vs]]
                        -> [VBool $ Set.null vs]
        , docs  = "Check if the given set is empty." }

   , PP { name  = "set'size"
        , tsig  = [("a", TData)] :*> ([TSet "a"] :-> [TNat])
        , step  = \[NTs [_], NVs [VSet _ vs]]
                        -> [VNat $ fromIntegral $ Set.size vs]
        , docs  = "Produce the size of the given set." }

   , PP { name  = "set'hasElem"
        , tsig  = [("a", TData)] :*> (["a", TSet "a"] :-> [TBool])
        , step  = \[NTs [_], NVs [v, VSet _ vs]]
                        -> [VBool $ Set.member (stripAnnot v) vs]
        , docs  = "Check if an element is in the given set." }

   , PP { name  = "set'insert"
        , tsig  = [("a", TData)] :*> (["a", TSet "a"] :-> [TSet "a"])
        , step  = \[NTs [t], NVs [v, VSet _ vs]]
                        -> [VSet t $ Set.insert (stripAnnot v) vs]
        , docs  = "Insert an element into a set." }

   , PP { name  = "set'delete"
        , tsig  = [("a", TData)] :*> (["a", TSet "a"] :-> [TSet "a"])
        , step  = \[NTs [t], NVs [v, VSet _ vs]]
                        -> [VSet t $ Set.delete (stripAnnot v) vs]
        , docs  = "Delete an element from a set." }
   ]


-- Map --------------------------------------------------------------------------------------------
primOpsMap
 = [ PP { name  = "map'empty"
        , tsig  = [("k", TData), ("v", TData)]
                        :*> ([] :-> [TMap "k" "v"])
        , step  = \[NTs [tk, tv], NVs []]
                        -> [VMap tk tv $ Map.empty]
        , docs  = "Construct an empty map." }

   , PP { name  = "map'isEmpty"
        , tsig  = [("k", TData), ("v", TData)]
                        :*> ([TMap "k" "v"] :-> [TBool])
        , step  = \[NTs [_], NVs [VMap _ _ vks]]
                        -> [VBool $ Map.null vks]
        , docs  = "Check if the given map is empty." }

   , PP { name  = "map'size"
        , tsig  = [("k", TData), ("v", TData)]
                        :*> ([TMap "k" "v"] :-> [TNat])
        , step  = \[NTs [_], NVs [VMap _ _ vks]]
                        -> [VNat $ fromIntegral $ Map.size vks]
        , docs  = "Produce the size of the given map." }

   , PP { name  = "map'insert"
        , tsig  = [("k", TData), ("v", TData)]
                        :*> (["k", "v", TMap "k" "v"] :-> [TMap "k" "v"])
        , step  = \[NTs [tk, tv], NVs [vk, vv, VMap _ _ vks]]
                        -> [VMap tk tv $ Map.insert (stripAnnot vk) vv vks]
        , docs  = "Insert an element into a map." }

   , PP { name  = "map'insert"
        , tsig  = [("k", TData), ("v", TData)]
                        :*> (["k", "v", TMap "k" "v"] :-> [TMap "k" "v"])
        , step  = \[NTs [tk, tv], NVs [vk, vv, VMap _ _ vks]]
                        -> [VMap tk tv $ Map.insert (stripAnnot vk) vv vks]
        , docs  = "Insert an element into a map." }

   , PP { name  = "map'delete"
        , tsig  = [("k", TData), ("v", TData)]
                        :*> (["k", TMap "k" "v"] :-> [TMap "k" "v"])
        , step  = \[NTs [tk, tv], NVs [vk, VMap _ _ vks]]
                        -> [VMap tk tv $ Map.delete (stripAnnot vk) vks]
        , docs  = "Delete an element from a map." }
   ]


-- Console ----------------------------------------------------------------------------------------
primOpsConsole
 = [ PO { name  = "console'print"
        , tsig  = [TText] :-> []
        , teff  = [TPrm "Console"]
        , exec  = \[NVs [VText tx]] -> do Text.putStr tx; return []
        , docs  = "Print a text string to the console." }

   , PO { name  = "console'println"
        , tsig  = [TText] :-> []
        , teff  = [TPrm "Console"]
        , exec  = \[NVs [VText tx]] -> do Text.putStrLn tx; return []
        , docs  = "Print a text string to the console, with a newline on the end." }
   ]


-- Debug ------------------------------------------------------------------------------------------
primOpsDebug
 = [ PO { name  = "debug'print'raw"
        , tsig  = [("a", TData)] :*> (["a"] :-> [TUnit])
        , teff  = []
        , exec  = \[NTs [_ta], NVs [v]] -> do putStrLn $ "TRACE " ++ show v; return [VUnit]
        , docs  = "DEBUG: Print the internal representaiton of a value to the local console." }
   ]

