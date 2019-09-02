
module Salt.Core.Prim.Ops.Map where
import Salt.Core.Prim.Ops.Base
import qualified Data.Map.Strict        as Map


primOpsMap
 = [ PP { name  = "map'empty"
        , tpms  = [("k", TData), ("v", TData)]
        , tsig  = TMap "k" "v"
        , step  = \[NTs [tk, tv]]
                -> [VMap tk tv $ Map.empty]
        , docs  = "Construct an empty map." }

   , PP { name  = "map'isEmpty"
        , tpms  = [("k", TData), ("v", TData)]
        , tsig  = [TMap "k" "v"] :-> [TBool]
        , step  = \[NTs [_, _], NVs [VMap _ _ vks]]
                -> [VBool $ Map.null vks]
        , docs  = "Check if the given map is empty." }

   , PP { name  = "map'size"
        , tpms  = [("k", TData), ("v", TData)]
        , tsig  = [TMap "k" "v"] :-> [TNat]
        , step  = \[NTs [_, _], NVs [VMap _ _ vks]]
                -> [VNat $ fromIntegral $ Map.size vks]
        , docs  = "Produce the size of the given map." }

   , PP { name  = "map'insert"
        , tpms  = [("k", TData), ("v", TData)]
        , tsig  = ["k", "v", TMap "k" "v"] :-> [TMap "k" "v"]
        , step  = \[NTs [tk, tv], NVs [vk, vv, VMap _ _ vks]]
                -> [VMap tk tv $ Map.insert (stripAnnot vk) vv vks]
        , docs  = "Insert an element into a map." }

   , PP { name  = "map'delete"
        , tpms  = [("k", TData), ("v", TData)]
        , tsig  = ["k", TMap "k" "v"] :-> [TMap "k" "v"]
        , step  = \[NTs [tk, tv], NVs [vk, VMap _ _ vks]]
                -> [VMap tk tv $ Map.delete (stripAnnot vk) vks]
        , docs  = "Delete an element from a map." }

   , PP { name  = "map'lookup"
        , tpms  = [("k", TData), ("v", TData)]
        , tsig  = ["k", TMap "k" "v"] :-> [TOption "v"]
        , step  = \[NTs [_, tv], NVs [vk, VMap _ _ vks]]
                -> case Map.lookup (stripAnnot vk) vks of
                        Nothing -> [VNone tv]
                        Just vv -> [VSome tv vv]
        , docs  = "Lookup an element from a map." }
   ]
