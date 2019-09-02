
module Salt.Core.Prim.Ops.Int where
import Salt.Core.Prim.Ops.Base


primOpsInt
 = [ PP { name  = "int'add"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int'sub"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int'mul"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int'div"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int'rem"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TInt]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VInt $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int'eq"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int'neq"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int'lt"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int'le"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int'gt"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int'ge"
        , tpms  = []
        , tsig  = [TInt, TInt] :-> [TBool]
        , step  = \[NVs [VInt n1, VInt n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]
