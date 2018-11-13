
module Salt.Core.Prim.Ops where
import Salt.Core.Transform.MapAnnot
import Salt.Core.Exp
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import Data.Map                         (Map)


---------------------------------------------------------------------------------------------------
-- | Holds information about a primitive operator.
--   We keep all the info about an operator in once place,
--   instead of spread out all over the compiler and external documentation.
data Prim
        -- Define a pure primitive operator.
        -- Provided the values match the expected types,
        -- these operators always succeed, and perform no actions.
        = PP
        { name  :: Name
        , tsig  :: Type ()
        , step  :: forall a. [Type a] -> [Value a] -> [Value a]
        , docs  :: Text }

        -- Define an operator that performs an action in the local process.
        -- These are only used for debugging.
        | PO
        { name  :: Name
        , tsig  :: Type ()
        , exec  :: forall a. Show a => [Type a] -> [Value a] -> IO [Value a]
        , docs  :: Text }


typeOfPrim :: Prim -> Type ()
typeOfPrim pp
 = case pp of
        PP {tsig} -> tsig
        PO {tsig} -> tsig


primOps :: Map Name Prim
primOps
 = Map.fromList $ map (\p -> (name p, p)) $ concat
        [ primOpsBool,   primOpsNat
        , primOpsSymbol
        , primOpsList,   primOpsSet
        , primOpsDebug ]


-- Bool -------------------------------------------------------------------------------------------
primOpsBool
 = [ PP { name  = "bool'not"
        , tsig  = [TBool] :-> [TBool]
        , step  = \[] [VBool b] -> [VBool (not b)]
        , docs  = "Boolean negation." }

   , PP { name  = "bool'and"
        , tsig  = [TBool, TBool] :-> [TBool]
        , step  = \[] [VBool b1, VBool b2] -> [VBool $ b1 && b2]
        , docs  = "Boolean and." }

   , PP { name  = "bool'or"
        , tsig  = [TBool, TBool] :-> [TBool]
        , step  = \[] [VBool b1, VBool b2] -> [VBool $ b1 || b2]
        , docs  = "Boolean or." }

  , PP { name  = "bool'eq"
        , tsig  = [TBool, TBool] :-> [TBool]
        , step  = \[] [VBool b1, VBool b2] -> [VBool $ b1 == b2]
        , docs  = "Boolean or." }
   ]


-- Nat --------------------------------------------------------------------------------------------
primOpsNat
 = [ PP { name  = "nat'add"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[] [VNat n1, VNat n2] -> [VNat $ n1 + n2]
        , docs  = "Natural number addition." }

   , PP { name  = "nat'sub"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[] [VNat n1, VNat n2] -> [VNat $ n1 - n2]
        , docs  = "Natural number subtraction." }

   , PP { name  = "nat'mul"
        , tsig  = [TNat, TNat] :-> [TNat]
        , step  = \[] [VNat n1, VNat n2] -> [VNat $ n1 * n2]
        , docs  = "Natural number multiplication." }

   , PP { name  = "nat'eq"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[] [VNat n1, VNat n2] -> [VBool $ n1 == n2]
        , docs  = "Natural number equality." }

   , PP { name  = "nat'neq"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[] [VNat n1, VNat n2] -> [VBool $ n1 /= n2]
        , docs  = "Natural number negated equality." }

   , PP { name  = "nat'lt"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[] [VNat n1, VNat n2] -> [VBool $ n1 < n2]
        , docs  = "Natural number less-than." }

   , PP { name  = "nat'le"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[] [VNat n1, VNat n2] -> [VBool $ n1 <= n2]
        , docs  = "Natural number less-than or equal." }

   , PP { name  = "nat'gt"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[] [VNat n1, VNat n2] -> [VBool $ n1 > n2]
        , docs  = "Natural number greater-than." }

   , PP { name  = "nat'ge"
        , tsig  = [TNat, TNat] :-> [TBool]
        , step  = \[] [VNat n1, VNat n2] -> [VBool $ n1 >= n2]
        , docs  = "Natural number greater-than or equal." }
   ]


-- Symbol -----------------------------------------------------------------------------------------
primOpsSymbol
 = [ PP { name  = "symbol'eq"
        , tsig  = [TSymbol, TSymbol] :-> [TBool]
        , step  = \[] [VSymbol s1, VSymbol s2] -> [VBool $ s1 == s2]
        , docs  = "Symbol equality comparison." }
   ]


-- List -------------------------------------------------------------------------------------------
primOpsList
 = [ PP { name  = "list'empty"
        , tsig  = [("a", TData)] :*> TList "a"
        , step  = \[t] [] -> [VList t []]
        , docs  = "Construct an empty list" }

   , PP { name  = "list'nil"
        , tsig  = [("a", TData)] :*> TList "a"
        , step  = \[t] [] -> [VList t []]
        , docs  = "Construct an empty list (same as #list'empty)." }

   , PP { name  = "list'cons"
        , tsig  = [("a", TData)] :*> (["a", TList "a"] :-> [TList "a"])
        , step  = \[t] [v, VList _ vs] -> [VList t (v : vs)]
        , docs  = "Attach an element to the front of an existing list." }

   , PP { name  = "list'isEmpty"
        , tsig  = [("a", TData)] :*> ([TList "a"] :-> [TBool])
        , step  = \[_] [VList _ vs] -> [VBool $ null vs]
        , docs  = "Check if the given list is empty." }

   , PP { name  = "list'size"
        , tsig  = [("a", TData)] :*> ([TList "a"] :-> [TNat])
        , step  = \[_] [VList _ vs] -> [VNat $ fromIntegral $ length vs]
        , docs  = "Produce the size of the given list." }

   , PP { name  = "list'head"
        , tsig  = [("a", TData)] :*> ([TList "a"] :-> [TOption "a"])
        , step  = \[t] [VList _ vs] -> case vs of { [] -> [VNone t]; v : _ -> [VSome t v] }
        , docs  = "Take the head element of a list." }

   , PP { name  = "list'tail"
        , tsig  = [("a", TData)] :*> ([TList "a"] :-> [TOption (TList "a")])
        , step  = \[t] [VList _ vs] -> case vs of { [] -> [VNone t]; v : _ -> [VSome t v] }
        , docs  = "Take the head element of a list." }
   ]


-- Set --------------------------------------------------------------------------------------------
primOpsSet
 = [ PP { name  = "set'empty"
        , tsig  = [("a", TData)] :*> ([] :-> [TSet "a"])
        , step  = \[t] [] -> [VSet t $ Set.empty]
        , docs  = "Construct an empty set." }

   , PP { name  = "set'fromList"
        , tsig  = [("a", TData)] :*> (["a"] :-> [TList "a"])
        , step  = \[t] [VList _ vs] -> [VSet t $ Set.fromList $ map stripAnnot vs]
        , docs  = "Construct a set from a list of values." }

   , PP { name  = "set'isEmpty"
        , tsig  = [("a", TData)] :*> ([TSet "a"] :-> [TBool])
        , step  = \[_] [VSet _ vs] -> [VBool $ Set.null vs]
        , docs  = "Check if the given set is empty." }

   , PP { name  = "set'size"
        , tsig  = [("a", TData)] :*> ([TSet "a"] :-> [TNat])
        , step  = \[_] [VSet _ vs] -> [VNat $ fromIntegral $ Set.size vs]
        , docs  = "Produce the size of the given set." }

   , PP { name  = "set'hasElem"
        , tsig  = [("a", TData)] :*> (["a", TSet "a"] :-> [TBool])
        , step  = \[_] [v, VSet _ vs] -> [VBool $ Set.member (stripAnnot v) vs]
        , docs  = "Check if an element is in the given set." }

   , PP { name  = "set'insert"
        , tsig  = [("a", TData)] :*> (["a", TSet "a"] :-> [TSet "a"])
        , step  = \[t] [v, VSet _ vs] -> [VSet t $ Set.insert (stripAnnot v) vs]
        , docs  = "Insert an element into a set." }

   , PP { name  = "set'delete"
        , tsig  = [("a", TData)] :*> (["a", TSet "a"] :-> [TSet "a"])
        , step  = \[t] [v, VSet _ vs] -> [VSet t $ Set.delete (stripAnnot v) vs]
        , docs  = "Delete an element from a set." }
   ]


-- Debug ------------------------------------------------------------------------------------------
primOpsDebug
 = [ PO { name  = "debug'print'raw"
        , tsig  = [("a", TData)] :*> (["a"] :-> [TUnit])
        , exec  = \[_ta] [v] -> do putStrLn $ "TRACE " ++ show v; return [VUnit]
        , docs  = "DEBUG: Print the internal representaiton of a value to the local console." }
   ]

