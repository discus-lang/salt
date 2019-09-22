
module Salt.Core.Prim.Ops.Bundle where
import Salt.Core.Prim.Ops.Base


primOpsBundle
 = [ PP { name  = "bundle'new"
        , tsig  = [TSet TSymbol, TSet TSymbol] :-> [TBundle]
        , step  = \_ -> error "primOpsBundle: #bundle'new handled in evaluator"
        , docs  = "Construct a new code bundle." }
   ]
