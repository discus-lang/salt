
module Salt.Core.Prim.Ops.Word32 where
import Salt.Core.Prim.Ops.Base


primOpsWord32
 = [ PP { name  = "word32'add"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 + n2]
        , docs  = "Word addition." }

   , PP { name  = "word32'sub"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 - n2]
        , docs  = "Word subtraction." }

   , PP { name  = "word32'mul"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 * n2]
        , docs  = "Word multiplication." }

   , PP { name  = "word32'div"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 `div` n2]
        , docs  = "Word division." }

   , PP { name  = "word32'rem"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TWord32]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VWord32 $ n1 `rem` n2]
        , docs  = "Word remainder." }

   , PP { name  = "word32'eq"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 == n2]
        , docs  = "Word equality." }

   , PP { name  = "word32'neq"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Word negated equality." }

   , PP { name  = "word32'lt"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 < n2]
        , docs  = "Word less-than." }

   , PP { name  = "word32'le"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Word less-than or equal." }

   , PP { name  = "word32'gt"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 > n2]
        , docs  = "Word greater-than." }

   , PP { name  = "word32'ge"
        , tpms  = []
        , tsig  = [TWord32, TWord32] :-> [TBool]
        , step  = \[NVs [VWord32 n1, VWord32 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Word greater-than or equal." }
   ]
