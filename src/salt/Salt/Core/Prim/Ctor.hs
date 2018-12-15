
module Salt.Core.Prim.Ctor where
import Salt.Core.Exp
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map

-- | Sorts of primitive kind constructors.
primKindCtors :: Map Name ()
primKindCtors
 = Map.fromList
        [ ("Data",      ())
        , ("Region",    ())
        , ("Effect",    ()) ]


-- | Kinds of primitive type constructors.
primTypeCtors :: Map Name (Type ())
primTypeCtors
 = Map.fromList
        [ ("Unit",      TData)
        , ("Bool",      TData)
        , ("Nat",       TData)
        , ("Int",       TData)
        , ("Text",      TData)
        , ("Symbol",    TData)
        , ("Option",    [TData] :=> TData)
        , ("List",      [TData] :=> TData)
        , ("Set",       [TData] :=> TData)
        , ("Map",       [TData, TData] :=> TData)
        , ("Console",   TEffect)
        , ("Sleep",     TEffect) ]


-- | Types of primitive data constructors.
primDataCtors :: Map Name (Type ())
primDataCtors
 = Map.fromList
        [ ("None",      [("a", TData)] :*> ([] :-> [TOption "a"]))
        , ("Some",      [("a", TData)] :*> (["a"] :-> [TOption "a"]))]

