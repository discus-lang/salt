
module Salt.Core.Prim.Ops.Symbol where
import Salt.Core.Prim.Ops.Base


primOpsSymbol
 = [ PP { name  = "symbol'eq"
        , tsig  = [TSymbol, TSymbol] :-> [TBool]
        , step  = \[NVs [VSymbol s1, VSymbol s2]] -> [VBool $ s1 == s2]
        , docs  = "Symbol equality comparison." }
   ]
