
module Salt.Core.Prim.Ops.Nat where
import Salt.Core.Prim.Ops.Base
import qualified Data.Text      as T

primOpsNat
 = [ PP { name  = "nat'show"
        , tpms  = []
        , tsig  = [TNat] :-> [TText]
        , step  = \[NVs [VNat n]] -> [VText $ T.pack $ show n]
        , docs  = "Convert nat to text." }

   , PP { name  = "nat'add"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 + n2]
        , docs  = "Natural number addition." }

   , PP { name  = "nat'sub"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 - n2]
        , docs  = "Natural number subtraction." }

   , PP { name  = "nat'mul"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 * n2]
        , docs  = "Natural number multiplication." }

   , PP { name  = "nat'div"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 `div` n2]
        , docs  = "Natural number division." }

   , PP { name  = "nat'rem"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VNat $ n1 `rem` n2]
        , docs  = "Natural number remainder." }

   , PP { name  = "nat'eq"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 == n2]
        , docs  = "Natural number equality." }

   , PP { name  = "nat'neq"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 /= n2]
        , docs  = "Natural number negated equality." }

   , PP { name  = "nat'lt"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 < n2]
        , docs  = "Natural number less-than." }

   , PP { name  = "nat'le"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 <= n2]
        , docs  = "Natural number less-than or equal." }

   , PP { name  = "nat'gt"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 > n2]
        , docs  = "Natural number greater-than." }

   , PP { name  = "nat'ge"
        , tpms  = []
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[NVs [VNat n1, VNat n2]] -> [VBool $ n1 >= n2]
        , docs  = "Natural number greater-than or equal." }
   ]
