
module Salt.Core.Prim.Ops.Word64 where
import Salt.Core.Prim.Ops.Base
import Data.Text (pack)


primOpsWord64
 = [ PP { name  = "word64'show"
        , tsig  = [TWord64] :-> [TText]
        , step  = \[NVs [VWord64 n]] -> [VText $ pack $ show n]
        , docs  = "Convert word to text." }

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
