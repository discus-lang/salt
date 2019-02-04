
module Salt.Core.Prim.Ops.Int where
import Salt.Core.Prim.Ops.Base


primOpsInt
 = [ PP { name  = "int'add"
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
