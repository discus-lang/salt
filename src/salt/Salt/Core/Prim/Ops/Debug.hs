
module Salt.Core.Prim.Ops.Debug where
import Salt.Core.Prim.Ops.Base


primOpsDebug
 = [ PO { name  = "debug'print'raw"
        , tsig  = [("a", TData)] :*> ["a"] :-> [TUnit]
        , exec  = \[NTs [_ta], NVs [v]] -> do putStrLn $ "TRACE " ++ show v; return [VUnit]
        , docs  = "DEBUG: Print the internal representaiton of a value to the local console." }
   ]

