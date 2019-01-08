
module Salt.Core.Exp.Term.Predicates where
import Salt.Core.Exp.Term.Base


-- | Check if this the boolean true value.
isVTrue :: Value a -> Bool
isVTrue (VBool True)    = True
isVTrue _               = False


