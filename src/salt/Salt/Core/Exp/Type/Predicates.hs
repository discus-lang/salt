
module Salt.Core.Exp.Type.Predicates where
import Salt.Core.Exp.Type.Patterns
import Salt.Core.Exp.Type.Base


-- | Check if this is the data kind,
isTData :: Type a -> Bool
isTData tt
 = case tt of
        TAnn _ t -> isTData t
        TData    -> True
        _        -> False


-- | Check if this type is the pure effect.
isTPure :: Type a -> Bool
isTPure tt
 = case tt of
        TAnn _ t -> isTPure t
        TPure    -> True
        TSum []  -> True
        _        -> False


-- | Check if this type is a suspension.
isTSusp :: Type a -> Bool
isTSusp tt
 = case tt of
        TAnn _ t  -> isTSusp t
        TSusp _ _ -> True
        _         -> False


