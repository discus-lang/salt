
module Salt.Core.Check.Type.App where
import Salt.Core.Check.Type.Base


-- | Check the application of a type to some types.
checkTypeAppTypes
        :: Annot a => a -> [Where a]
        -> Context a -> a -> Kind a -> TypeArgs a
        -> IO (TypeArgs a, Kind a)

checkTypeAppTypes _a wh ctx aFun kFun (TGAnn a tgs')
 = checkTypeAppTypes a wh ctx aFun kFun tgs'

checkTypeAppTypes a  wh ctx aFun kFun (TGTypes tsArg)
 = case kFun of
        TArr ksParam kResult
          -> goCheckArgs ksParam kResult
        _ -> throw $ ErrorAppTypeTypeCannot aFun wh kFun
 where
        goCheckArgs ksParam kResult
         = if length ksParam /= length tsArg
             then throw $ ErrorAppTypeTypeWrongArityNum a wh ksParam (length tsArg)
             else do
                (tsArg', ksArg) <- checkTypes a wh ctx tsArg
                goCheckParams ksParam kResult (TGTypes tsArg') ksArg

        goCheckParams ksParam kResult tsArg' ksArg
         = checkTypeEquivs ctx a [] ksParam a [] ksArg
         >>= \case
                Nothing -> return (tsArg', kResult)
                Just ((_aErrParam, kErrParam), (aErrArg', kErrArg))
                 -> throw $ ErrorMismatch UKind aErrArg' wh kErrArg kErrParam
