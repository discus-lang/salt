
module Salt.Core.Prim.Ops.Word8 where
import Salt.Core.Prim.Ops.Base


primOpsWord8
 = [ PP { name  = "word8'add"
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


