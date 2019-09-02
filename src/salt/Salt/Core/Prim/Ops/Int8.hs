
module Salt.Core.Prim.Ops.Int8 where
import Salt.Core.Prim.Ops.Base
import Data.Text (pack)


primOpsInt8
 = [ PP { name  = "int8'show"
        , tpms  = []
        , tsig  = [TInt8] :-> [TText]
        , step  = \[NVs [VInt8 n]] -> [VText $ pack $ show n]
        , docs  = "Convert int to text." }

   , PP { name  = "int8'add"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int8'sub"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int8'mul"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int8'div"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int8'rem"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int8'eq"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int8'neq"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int8'lt"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int8'le"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int8'gt"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int8'ge"
        , tpms  = []
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]
