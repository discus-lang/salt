
module Salt.Core.Prim.Ops.Set where
import Salt.Core.Prim.Ops.Base
import qualified Data.Set       as Set


primOpsSet
 = [ PP { name  = "set'empty"
        , tsig  = [("a", TData)] :*> TSet "a"
        , step  = \[NTs [t]]
                -> [VSet t $ Set.empty]
        , docs  = "Construct an empty set." }

   , PP { name  = "set'fromList"
        , tsig  = [("a", TData)] :*> [TList "a"] :-> [TSet "a"]
        , step  = \[NTs [t], NVs [VList _ vs]]
                -> [VSet t $ Set.fromList $ map stripAnnot vs]
        , docs  = "Construct a set from a list of values." }

   , PP { name  = "set'isEmpty"
        , tsig  = [("a", TData)] :*> [TSet "a"] :-> [TBool]
        , step  = \[NTs [_], NVs [VSet _ vs]]
                        -> [VBool $ Set.null vs]
        , docs  = "Check if the given set is empty." }

   , PP { name  = "set'size"
        , tsig  = [("a", TData)] :*> [TSet "a"] :-> [TNat]
        , step  = \[NTs [_], NVs [VSet _ vs]]
                        -> [VNat $ fromIntegral $ Set.size vs]
        , docs  = "Produce the size of the given set." }

   , PP { name  = "set'hasElem"
        , tsig  = [("a", TData)] :*> ["a", TSet "a"] :-> [TBool]
        , step  = \[NTs [_], NVs [v, VSet _ vs]]
                        -> [VBool $ Set.member (stripAnnot v) vs]
        , docs  = "Check if an element is in the given set." }

   , PP { name  = "set'insert"
        , tsig  = [("a", TData)] :*> ["a", TSet "a"] :-> [TSet "a"]
        , step  = \[NTs [t], NVs [v, VSet _ vs]]
                        -> [VSet t $ Set.insert (stripAnnot v) vs]
        , docs  = "Insert an element into a set." }

   , PP { name  = "set'delete"
        , tsig  = [("a", TData)] :*> ["a", TSet "a"] :-> [TSet "a"]
        , step  = \[NTs [t], NVs [v, VSet _ vs]]
                        -> [VSet t $ Set.delete (stripAnnot v) vs]
        , docs  = "Delete an element from a set." }
   ]

