
module Salt.Core.Check.Term.Bind where
import Salt.Core.Check.Type.Base
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Term.Params


-- | Check a `TermBind`.
checkTermBind
        :: Annot a => a -> [Where a]
        -> Context a -> TermBind a -> IO (TermBind a)

checkTermBind a wh ctx (MBind b mpss tResult mBody)
 = do   -- Check the parameters.
        (ctx', mpss')
         <- checkTermParamss a wh ctx mpss

        -- There must be at least one vector of term parameters,
        -- as we do not support value recursion in the evaluator.
        when (not $ any isJust $ map takeMPTerms mpss)
         $ throw $ ErrorRecValueRecursion a wh b

        -- Check the result type annotation.
        tResult'
         <- checkTypeHas UKind a wh ctx' TData tResult

        -- The body must have type as specified by the result annotation.
        (mBody', _tsResult, esBody)
         <- checkTerm a wh ctx' (Check [tResult]) mBody

        -- The body must be pure.
        let aBody  = fromMaybe a $ takeAnnotOfTerm mBody
        eBody_red  <- simplType aBody ctx' (TSum esBody)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorAbsImpure UTerm aBody wh eBody_red

        return $ MBind b mpss' tResult' mBody'


-- | Make the type of a `TermBind`.
makeTypeOfTermBind :: TermBind a -> Type a
makeTypeOfTermBind (MBind _b mpss tResult _mBody)
 = loop mpss
 where
        loop [] = tResult

        loop (MPAnn _ mps' : pss')
         = loop (mps' : pss')

        loop (MPTerms bts : pss')
         = TFun (map snd bts) [loop pss']

        loop (MPTypes bts : pss')
         = TForall (TPTypes bts) $ loop pss'

