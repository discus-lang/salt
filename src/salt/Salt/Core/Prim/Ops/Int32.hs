
module Salt.Core.Prim.Ops.Int32 where
import Salt.Core.Prim.Ops.Base


primOpsInt32
 = [ PP { name  = "int32'add"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int32'sub"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int32'mul"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int32'div"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int32'rem"
        , tsig  = [TInt32, TInt32] :-> [TInt32]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VInt32 $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int32'eq"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int32'neq"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int32'lt"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int32'le"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int32'gt"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int32'ge"
        , tsig  = [TInt32, TInt32] :-> [TBool]
        , step  = \[NVs [VInt32 n1, VInt32 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]
