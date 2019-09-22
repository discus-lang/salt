
module Salt.Core.Prim.Ops.Word32 where
import Salt.Core.Prim.Ops.Base


primOpsWord32
 = [ PP { name  = "word32'add"
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
