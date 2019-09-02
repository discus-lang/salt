
module Salt.Core.Prim.Ops.Set where
import Salt.Core.Prim.Ops.Base
import qualified Data.Set       as Set


primOpsSet
 = [ PP { name  = "set'empty"
        , tpms  = [("a", TData)]
        , tsig  = TSet "a"
        , step  = \[NTs [t]]
                -> [VSet t $ Set.empty]
        , docs  = "Construct an empty set." }

   , PP { name  = "set'fromList"
        , tpms  = [("a", TData)]
        , tsig  = [TList "a"] :-> [TSet "a"]
        , step  = \[NTs [t], NVs [VList _ vs]]
                -> [VSet t $ Set.fromList $ map stripAnnot vs]
        , docs  = "Construct a set from a list of values." }

   , PP { name  = "set'isEmpty"
        , tpms  = [("a", TData)]
        , tsig  = [TSet "a"] :-> [TBool]
        , step  = \[NTs [_], NVs [VSet _ vs]]
                        -> [VBool $ Set.null vs]
        , docs  = "Check if the given set is empty." }

   , PP { name  = "set'size"
        , tpms  = [("a", TData)]
        , tsig  = [TSet "a"] :-> [TNat]
        , step  = \[NTs [_], NVs [VSet _ vs]]
                        -> [VNat $ fromIntegral $ Set.size vs]
        , docs  = "Produce the size of the given set." }

   , PP { name  = "set'hasElem"
        , tpms  = [("a", TData)]
        , tsig  = ["a", TSet "a"] :-> [TBool]
        , step  = \[NTs [_], NVs [v, VSet _ vs]]
                        -> [VBool $ Set.member (stripAnnot v) vs]
        , docs  = "Check if an element is in the given set." }

   , PP { name  = "set'insert"
        , tpms  = [("a", TData)]
        , tsig  = ["a", TSet "a"] :-> [TSet "a"]
        , step  = \[NTs [t], NVs [v, VSet _ vs]]
                        -> [VSet t $ Set.insert (stripAnnot v) vs]
        , docs  = "Insert an element into a set." }

   , PP { name  = "set'delete"
        , tpms  = [("a", TData)]
        , tsig  = ["a", TSet "a"] :-> [TSet "a"]
        , step  = \[NTs [t], NVs [v, VSet _ vs]]
                        -> [VSet t $ Set.delete (stripAnnot v) vs]
        , docs  = "Delete an element from a set." }
   ]

