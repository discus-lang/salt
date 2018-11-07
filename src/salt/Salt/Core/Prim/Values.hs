
module Salt.Core.Prim.Values where
import Salt.Core.Exp


-- | Take a primitive value from a name.
--   This is used by the parser to decide if a primitive is an operator
--   that should be applied, or is a value
takePrimValueOfName :: Name -> Maybe (Value a)
takePrimValueOfName (Name tx)
 = case tx of
        "unit"          -> Just $ VUnit
        "true"          -> Just $ VBool True
        "false"         -> Just $ VBool False

        "bool'true"     -> Just $ VBool True
        "bool'false"    -> Just $ VBool False

        _               -> Nothing

