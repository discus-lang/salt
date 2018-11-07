
module Salt.Core.Prim.Data where
import Salt.Core.Exp
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map


-- | Kinds of primitive type constructors.
primTypeCtors :: Map Name (Type ())
primTypeCtors
 = Map.fromList
        [ ("Nat",       TData)
        , ("Unit",      TData)
        , ("Bool",      TData)
        , ("Nat",       TData)
        , ("Int",       TData)
        , ("Text",      TData)
        , ("Symbol",    TData)
        , ("Maybe",     [TData] :--> TData)
        , ("List",      [TData] :--> TData)
        , ("Set",       [TData] :--> TData)
        , ("Map",       [TData, TData] :--> TData) ]


-- | Types of primitive data constructors.
primDataCtors :: Map Name (Type ())
primDataCtors
 = Map.fromList
        [ ("Nothing",   [("a", TData)] :*> ([] :-> [TMaybe "a"]))
        , ("Just",      [("a", TData)] :*> (["a"] :-> [TMaybe "a"]))]


