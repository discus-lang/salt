
module Salt.Core.Prim.Ops.Bool where
import Salt.Core.Prim.Ops.Base


primOpsBool
 = [ PP { name  = "bool'not"
        , tsig  = [TBool] :-> [TBool]
        , step  = \[NVs [VBool b]] -> [VBool (not b)]
        , docs  = "Boolean negation." }

   , PP { name  = "bool'and"
        , tsig  = [TBool, TBool] :-> [TBool]
        , step  = \[NVs [VBool b1, VBool b2]] -> [VBool $ b1 && b2]
        , docs  = "Boolean and." }

   , PP { name  = "bool'or"
        , tsig  = [TBool, TBool] :-> [TBool]
        , step  = \[NVs [VBool b1, VBool b2]] -> [VBool $ b1 || b2]
        , docs  = "Boolean or." }

  , PP  { name  = "bool'eq"
        , tsig  = [TBool, TBool] :-> [TBool]
        , step  = \[NVs [VBool b1, VBool b2]] -> [VBool $ b1 == b2]
        , docs  = "Boolean or." }
   ]
