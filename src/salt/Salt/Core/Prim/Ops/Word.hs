
module Salt.Core.Prim.Ops.Word where
import Salt.Core.Prim.Ops.Base


primOpsWord
 = [ PP { name  = "word'add"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 + n2]
        , docs  = "Word addition." }

   , PP { name  = "word'sub"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 - n2]
        , docs  = "Word subtraction." }

   , PP { name  = "word'mul"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 * n2]
        , docs  = "Word multiplication." }

   , PP { name  = "word'div"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 `div` n2]
        , docs  = "Word division." }

   , PP { name  = "word'rem"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TWord]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VWord $ n1 `rem` n2]
        , docs  = "Word remainder." }

   , PP { name  = "word'eq"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 == n2]
        , docs  = "Word equality." }

   , PP { name  = "word'neq"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 /= n2]
        , docs  = "Word negated equality." }

   , PP { name  = "word'lt"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 < n2]
        , docs  = "Word less-than." }

   , PP { name  = "word'le"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 <= n2]
        , docs  = "Word less-than or equal." }

   , PP { name  = "word'gt"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 > n2]
        , docs  = "Word greater-than." }

   , PP { name  = "word'ge"
        , tpms  = []
        , tsig  = [TWord, TWord] :-> [TBool]
        , step  = \[NVs [VWord n1, VWord n2]] -> [VBool $ n1 >= n2]
        , docs  = "Word greater-than or equal." }
   ]
