
module Salt.Core.Check.Type.App where
import Salt.Core.Check.Type.Base


-- | Check the application of a type to some types.
checkTypeAppTypes
        :: Annot a => a -> [Where a]
        -> Context a -> Kind a -> [Type a] -> IO ([Type a], Kind a)

checkTypeAppTypes a wh ctx kFun tsArg
 = case kFun of
        TArr ksParam kResult
          -> goCheckArgs ksParam kResult
        _ -> throw $ ErrorAppTypeTypeCannot a wh kFun
 where
        goCheckArgs ksParam kResult
         = if length ksParam /= length tsArg
             then throw $ ErrorAppTypeTypeWrongArityNum aArg wh ksParam (length tsArg)
             else do
                (tsArg', ksArg) <- checkTypes a wh ctx tsArg
                goCheckParams ksParam kResult tsArg' ksArg

        goCheckParams ksParam kResult tsArg' ksArg
         = checkTypeEquivs ctx a [] ksParam aArg [] ksArg
         >>= \case
                Nothing -> return (tsArg', kResult)
                Just ((_aErrParam, kErrParam), (aErrArg', kErrArg))
                 -> throw $ ErrorTypeMismatch aErrArg' wh kErrArg kErrParam

        -- Get the location of the argument vectors.
        aArg    = fromMaybe a 
                $ join $ fmap listToMaybe 
                $ sequence $ map takeAnnotOfType tsArg

