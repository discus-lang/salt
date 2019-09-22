
module Salt.Core.Prim.Ops.Int8 where
import Salt.Core.Prim.Ops.Base
import Data.Text (pack)


primOpsInt8
 = [ PP { name  = "int8'show"
        , tsig  = [TInt8] :-> [TText]
        , step  = \[NVs [VInt8 n]] -> [VText $ pack $ show n]
        , docs  = "Convert int to text." }

   , PP { name  = "int8'add"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 + n2]
        , docs  = "Integer addition." }

   , PP { name  = "int8'sub"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 - n2]
        , docs  = "Integer subtraction." }

   , PP { name  = "int8'mul"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 * n2]
        , docs  = "Integer multiplication." }

   , PP { name  = "int8'div"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 `div` n2]
        , docs  = "Integer division." }

   , PP { name  = "int8'rem"
        , tsig  = [TInt8, TInt8] :-> [TInt8]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VInt8 $ n1 `rem` n2]
        , docs  = "Integer remainder." }

   , PP { name  = "int8'eq"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 == n2]
        , docs  = "Integer equality." }

   , PP { name  = "int8'neq"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 /= n2]
        , docs  = "Integer negated equality." }

   , PP { name  = "int8'lt"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 < n2]
        , docs  = "Integer less-than." }

   , PP { name  = "int8'le"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 <= n2]
        , docs  = "Integer less-than or equal." }

   , PP { name  = "int8'gt"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 > n2]
        , docs  = "Integer greater-than." }

   , PP { name  = "int8'ge"
        , tsig  = [TInt8, TInt8] :-> [TBool]
        , step  = \[NVs [VInt8 n1, VInt8 n2]] -> [VBool $ n1 >= n2]
        , docs  = "Integer greater-than or equal." }
   ]
