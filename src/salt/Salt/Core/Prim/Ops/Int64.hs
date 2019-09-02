
module Salt.Core.Prim.Ops.Int64 where
import Salt.Core.Prim.Ops.Base


primOpsInt64
 = [ PP { name  = "int64'add"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int64'sub"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int64'mul"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int64'div"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int64'rem"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TInt64]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VInt64 $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int64'eq"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int64'neq"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int64'lt"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int64'le"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int64'gt"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int64'ge"
        , tpms  = []
        , tsig  = [TInt64, TInt64] :-> [TBool]
        , step  = \[NVs [VInt64 n1, VInt64 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]
