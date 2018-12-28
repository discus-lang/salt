
module Salt.Core.Check.Term.App where
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base


-- | Check the application of a term to some types.
checkTermAppTypes
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Type a]
        -> IO ([Type a], Type a)

checkTermAppTypes a wh ctx tFun tsArg
 = do
        -- The funtion needs to have a forall type.
        tFun_red <- simplType a ctx tFun
        (bksParam, tResult)
         <- case tFun_red of
                TForall bksParam tResult
                  -> return (bksParam, tResult)
                _ -> throw $ ErrorAppTermTypeCannot a wh tFun

        -- Check the kinds of the arguments.
        (tsArg', ksArg)
         <- checkTypes a wh ctx tsArg

        -- The number of arguments must match the number of parameters.
        when (not $ length bksParam == length tsArg)
         $ throw $ ErrorAppTermTypeWrongArity a wh bksParam tsArg

        -- Check the parameter and argument kinds match.
        (checkTypeEqs ctx a [] (map snd bksParam) a [] ksArg
         >>= \case
                Nothing -> return ()
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                  -> throw $ ErrorTypeMismatch a wh tErr1 tErr2)

        -- Substitute arguments into the result type to instantiate
        -- the type scheme.
        let nts    = [ (n, t) | (BindName n, _k) <- bksParam | t <- tsArg ]
        let snv    = snvOfBinds nts
        let tSubst = snvApplyType upsEmpty snv tResult

        -- Return the checked argument types and the instantiated scheme.
        return  (tsArg', tSubst)


-- | Check the application of a functional term to a vector of argument terms.
--   The arguments may only produce a single value each.
checkTermAppTerms
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Term a]
        -> IO ([Term a], [Type a], [Effect a])

checkTermAppTerms a wh ctx tFun msArg
 = do
        -- The function needs to have a functional type.
        tFun_red <- simplType a ctx tFun
        let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot a wh tFun)
                $ takeTFun tFun_red

        -- The number of arguments must match the number of parameters.
        when (not $ length msArg == length tsParam)
         $ throw $ ErrorAppTermTermWrongArityNum a wh tsParam (length msArg)

        -- Check the types of the arguments.
        (msArg', tsArg, esArgs)
         <- checkTerms a wh ctx (Check tsParam) msArg

        -- Check the parameter and argument types match.
        checkTypeEqs ctx a [] tsParam a [] tsArg
         >>= \case
                Nothing -> return  (msArg', tsResult, esArgs)
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                 -> throw $ ErrorTypeMismatch a wh tErr1 tErr2


-- | Check the application of a functional term to an argument term.
--   Reduction of the argument may produce a vector of values.
checkTermAppTerm
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> Term a
        -> IO (Term a, [Type a], [Effect a])

checkTermAppTerm a wh ctx tFun mArg
 = do
        -- The function needs to have a functional type.
        tFun_red <- simplType a ctx tFun
        let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot a wh tFun)
                $ takeTFun tFun_red

        -- Check the types of the argument.
        (mArg', tsArg, esArg)
         <- checkTerm a wh ctx (Check tsParam) mArg

        -- The number of arguments must match the number of parameters.
        when (not $ length tsParam == length tsArg)
         $ throw $ ErrorAppTermTermWrongArity a wh tsParam tsArg

        -- Check the parameter and argument tpyes match.
        checkTypeEqs ctx a [] tsParam a [] tsArg
         >>= \case
                Nothing -> return (mArg', tsResult, esArg)
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                 -> throw $ ErrorTypeMismatch a wh tErr1 tErr2

