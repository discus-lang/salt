
module Salt.Core.Prim.Ops.Word16 where
import Salt.Core.Prim.Ops.Base


primOpsWord16
 = [ PP { name  = "word16'add"
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
