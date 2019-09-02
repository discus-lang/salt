
module Salt.Core.Prim.Ops.Int16 where
import Salt.Core.Prim.Ops.Base


primOpsInt16
 = [ PP { name  = "int16'add"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int16'sub"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int16'mul"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int16'div"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int16'rem"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TInt16]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VInt16 $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int16'eq"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int16'neq"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int16'lt"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int16'le"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int16'gt"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int16'ge"
        , tpms  = []
        , tsig  = [TInt16, TInt16] :-> [TBool]
        , step  = \[NVs [VInt16 n1, VInt16 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]
