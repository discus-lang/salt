
module Salt.Core.Check.Term.App where
import Salt.Core.Check.Type
import Salt.Core.Check.Term.Base
import qualified Data.Map       as Map


-- | Check the application of a term to some types.
checkTermAppTypes
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Type a]
        -> IO ([Type a], Type a)

checkTermAppTypes a wh ctx tFun tsArg
 = do
        -- The funtion needs to have a forall type.
        (bksParam, tResult)
         <- case tFun of
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
        (case checkTypeEqs a [] (map snd bksParam) a [] ksArg of
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                  -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
                _ -> return ())

        -- Substitute arguments into the result type to instantiate
        -- the type scheme.
        let subst   = Map.fromList
                        [ (n, t) | (BindName n, _k) <- bksParam
                                 | t <- tsArg ]
        let tSubst  = substTypeType [subst] tResult

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
        let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot a wh tFun)
                $ takeTFun tFun

        -- The number of arguments must match the number of parameters.
        when (not $ length msArg == length tsParam)
         $ throw $ ErrorAppTermTermWrongArityNum a wh tsParam (length msArg)

        -- Check the types of the arguments.
        (msArg', tsArg, esArgs)
         <- checkTerms a wh ctx (Check tsParam) msArg

        -- Check the parameter and argument types match.
        case checkTypeEqs a [] tsParam a [] tsArg of
         Just ((_aErr1', tErr1), (_aErr2', tErr2))
           -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
         _ -> return  (msArg', tsResult, esArgs)


-- | Check the application of a functional term to an argument term.
--   Reduction of the argument may produce a vector of values.
checkTermAppTerm
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> Term a
        -> IO (Term a, [Type a], [Effect a])

checkTermAppTerm a wh ctx tFun mArg
 = do
        -- The function needs to have a functional type.
        let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot a wh tFun)
                $ takeTFun tFun

        -- Check the types of the argument.
        (mArg', tsArg, esArg)
         <- checkTerm a wh ctx (Check tsParam) mArg

        -- The number of arguments must match the number of parameters.
        when (not $ length tsParam == length tsArg)
         $ throw $ ErrorAppTermTermWrongArity a wh tsParam tsArg

        -- Check the parameter and argument tpyes match.
        case checkTypeEqs a [] tsParam a [] tsArg of
         Just ((_aErr1', tErr1), (_aErr2', tErr2))
           -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
         _ -> return (mArg', tsResult, esArg)

