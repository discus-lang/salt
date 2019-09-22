
module Salt.Core.Prim.Ops
        ( Prim(..)
        , typeOfPrim
        , primOps)
where
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map

import Salt.Core.Prim.Ops.Base
import Salt.Core.Prim.Ops.Bool
import Salt.Core.Prim.Ops.Console
import Salt.Core.Prim.Ops.Debug
import Salt.Core.Prim.Ops.Int
import Salt.Core.Prim.Ops.Int8
import Salt.Core.Prim.Ops.Int16
import Salt.Core.Prim.Ops.Int32
import Salt.Core.Prim.Ops.Int64
import Salt.Core.Prim.Ops.List
import Salt.Core.Prim.Ops.Map
import Salt.Core.Prim.Ops.Memory
import Salt.Core.Prim.Ops.Nat
import Salt.Core.Prim.Ops.Set
import Salt.Core.Prim.Ops.Symbol
import Salt.Core.Prim.Ops.Word
import Salt.Core.Prim.Ops.Word8
import Salt.Core.Prim.Ops.Word16
import Salt.Core.Prim.Ops.Word32
import Salt.Core.Prim.Ops.Word64
import Salt.Core.Prim.Ops.Reify
import Salt.Core.Prim.Ops.Bundle


primOps :: Map Name Prim
primOps
 = Map.fromList $ map (\p -> (name p, p)) $ concat
        [ primOpsBool,  primOpsNat
        , primOpsInt,   primOpsInt8,  primOpsInt16,  primOpsInt32,  primOpsInt64
        , primOpsWord,  primOpsWord8, primOpsWord16, primOpsWord32, primOpsWord64
        , primOpsSymbol
        , primOpsList,   primOpsSet,  primOpsMap
        , primOpsMemory
        , primOpsDebug
        , primOpsConsole
        , primOpsReify
        , primOpsBundle ]


