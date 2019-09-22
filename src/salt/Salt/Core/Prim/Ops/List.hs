
module Salt.Core.Prim.Ops.List where
import Salt.Core.Prim.Ops.Base


primOpsList
 = [ PP { name  = "list'empty"
        , tsig  = [("a", TData)] :*> TList "a"
        , step  = \[NTs [t]] -> [VList t []]
        , docs  = "Construct an empty list" }

   , PP { name  = "list'nil"
        , tsig  = [("a", TData)] :*> TList "a"
        , step  = \[NTs [t]] -> [VList t []]
        , docs  = "Construct an empty list (same as #list'empty)." }

   , PP { name  = "list'one"
        , tsig  = [("a", TData)] :*> ["a"] :-> [TList "a"]
        , step  = \[NTs [t], NVs [v]] -> [VList t [v]]
        , docs  = "Construct a singleton list." }

   , PP { name  = "list'cons"
        , tsig  = [("a", TData)] :*> ["a", TList "a"] :-> [TList "a"]
        , step  = \[NTs [t], NVs [v, VList _ vs]]
                -> [VList t (v : vs)]
        , docs  = "Attach an element to the front of an existing list." }

   , PP { name  = "list'isEmpty"
        , tsig  = [("a", TData)] :*> [TList "a"] :-> [TBool]
        , step  = \[NTs [_], NVs [VList _ vs]]
                -> [VBool $ null vs]
        , docs  = "Check if the given list is empty." }

   , PP { name  = "list'size"
        , tsig  = [("a", TData)] :*> [TList "a"] :-> [TNat]
        , step  = \[NTs [_], NVs [VList _ vs]]
                -> [VNat $ fromIntegral $ length vs]
        , docs  = "Produce the size of the given list." }

   , PP { name  = "list'head"
        , tsig  = [("a", TData)] :*> [TList "a"] :-> [TOption "a"]
        , step  = \[NTs [t], NVs [VList _ vs]]
                -> case vs of { [] -> [VNone t]; v : _ -> [VSome t v] }
        , docs  = "Take the head element of a list." }

   , PP { name  = "list'tail"
        , tsig  = [("a", TData)] :*> [TList "a"] :-> [TOption (TList "a")]
        , step  = \[NTs [t], NVs [VList _ vs]]
                -> case vs of { [] -> [VNone t]; _ : vs' -> [VSome t (VList t vs')] }
        , docs  = "Take the head element of a list." }

   , PP { name  = "list'case"
        , tsig  = [("a", TData)] :*> [TList "a"] :-> [makeListType "a"]
        , step  = \ns
                -> case ns of
                        [NTs [t], NVs [VList _ vv]]
                         -> case vv of
                                []      -> [VVariant "nil"  (makeListType t) []]
                                v : vs  -> [VVariant "cons" (makeListType t) [v, VList t vs]]

                        _ -> error $ show ns
        , docs  = "Case analysis on a list." }
   ]

makeListType :: Type a -> Type a
makeListType tArg
 = TVariant
        ["nil", "cons"]
        [ TGTypes [], TGTypes [tArg, TList tArg] ]
