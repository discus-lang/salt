
module Salt.Core.Prim.Ops.Nat where
import Salt.Core.Prim.Ops.Base
import qualified Data.Text      as T

primOpsNat
 = [ PP { name  = "nat'show"
        , tsig  = [TNat] :-> [TText]
        , step  = \[NVs [VNat n]] -> [VText $ T.pack $ show n]
        , docs  = "Convert nat to text." }

   , PP { name  = "nat'add"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 + n2]
        , docs  = "Natural number addition." }

   , PP { name  = "nat'sub"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 - n2]
        , docs  = "Natural number subtraction." }

   , PP { name  = "nat'mul"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 * n2]
        , docs  = "Natural number multiplication." }

   , PP { name  = "nat'div"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 `div` n2]
        , docs  = "Natural number division." }

   , PP { name  = "nat'rem"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 `rem` n2]
        , docs  = "Natural number remainder." }

   , PP { name  = "nat'eq"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 == n2]
        , docs  = "Natural number equality." }

   , PP { name  = "nat'neq"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 /= n2]
        , docs  = "Natural number negated equality." }

   , PP { name  = "nat'lt"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 < n2]
        , docs  = "Natural number less-than." }

   , PP { name  = "nat'le"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 <= n2]
        , docs  = "Natural number less-than or equal." }

   , PP { name  = "nat'gt"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 > n2]
        , docs  = "Natural number greater-than." }

   , PP { name  = "nat'ge"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 >= n2]
        , docs  = "Natural number greater-than or equal." }
   ]
