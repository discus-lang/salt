
module Salt.Core.Exp.Term.Predicates where
import Salt.Core.Exp.Term.Patterns
import Salt.Core.Exp.Term.Base


-- | Check if this the boolean true value.
isVTrue :: Value a -> Bool
isVTrue (VBool True)    = True
isVTrue _               = False


-- | Check if this is a procedure, or one wrapped by a 'the'
isSomeMProc :: Term a -> Bool
isSomeMProc mm
 = case mm of
        MAnn _ m        -> isSomeMProc m
        MThe _ m        -> isSomeMProc m
        MSeq{}          -> True
        MLaunch{}       -> True
        MReturn{}       -> True
        MCell{}         -> True
        MUpdate{}       -> True
        MWhens{}        -> True
        MMatch{}        -> True
        MLoop{}         -> True
        MBreak{}        -> True
        MContinue{}     -> True
        MWhile{}        -> True
        MEnter{}        -> True
        MLeave{}        -> True
        _               -> False


