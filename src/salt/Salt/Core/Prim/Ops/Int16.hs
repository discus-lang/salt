
module Salt.Core.Prim.Ops.Int16 where
import Salt.Core.Prim.Ops.Base


primOpsInt16
 = [ PP { name  = "int16'add"
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
