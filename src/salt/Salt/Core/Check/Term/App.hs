
module Salt.Core.Check.Term.App where
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import qualified Salt.Core.Prim.Ops     as Prim
import qualified Salt.Core.Prim.Ctor    as Prim
import qualified Data.Map.Strict        as Map


-------------------------------------------------------------------------------------------- App --
-- | Check an application of a term to its arguments.
checkTermApp
        :: Annot a => a -> [Where a]
        -> Context a -> Term a -> [TermArgs a]
        -> IO (Term a, [Type a], [Effect a])

checkTermApp a wh ctx mFun0 mgss0
 = do   -- If this an effectful primitive then also add the effects
        -- we get from applying it.
        (mFun1, tFun1, esFun1)
         <- case takeMPrm mFun0 of
                Just nPrm
                 | Just pp  <- Map.lookup nPrm Prim.primOps
                 -> do
                        let tPrim = mapAnnot (const a) $ Prim.typeOfPrim pp
                        let ePrim = mapAnnot (const a) $ Prim.effectOfPrim pp

                        pss <- stripTermParamsOfType ctx tPrim
                        when (length pss /= length mgss0)
                         $ throw $ ErrorUnsaturatedPrim a wh nPrm tPrim

                        return (mFun0, tPrim, [ePrim])

                 | Just tCon <- Map.lookup nPrm Prim.primDataCtors
                 -> do
                        let tCon' = mapAnnot (const a) tCon

                        pss <- stripTermParamsOfType ctx tCon'
                        when (length pss /= length mgss0)
                         $ throw $ ErrorUnsaturatedCtor a wh nPrm tCon'

                        return (mFun0, tCon', [])

                 | otherwise
                 -> let aFun = fromMaybe a (takeAnnotOfTerm mFun0)
                    in  throw $ ErrorUnknownPrim UTerm aFun wh nPrm

                Nothing
                 -> checkTerm1 a wh ctx Synth mFun0

        -- Check that we have at least some arguments to apply.
        when (null mgss0)
         $ throw $ ErrorAppNoArguments a wh tFun1

        let aFun = fromMaybe a $ takeAnnotOfTerm mFun0

        -- Check argument types line up with parameter types.
        let -- (t-apt) -----
            checkApp [tFun] es (mps : mgss) mgssAcc
             | Just (a', tsArg) <- takeAnnMGTypes a mps
             = do (tsArg', tResult)  <- checkTermAppTypes a' wh ctx aFun tFun tsArg
                  checkApp [tResult] es mgss (MGAnn a' (MGTypes tsArg') : mgssAcc)

            -- (t-apm) -----
            checkApp [tFun] es (mps : mgss) mgssAcc
             | Just (a', msArg) <- takeAnnMGTerms a mps
             = do (msArg', tsResult, es') <- checkTermAppTerms a' wh ctx aFun tFun msArg
                  checkApp tsResult (es ++ es') mgss (MGAnn a' (MGTerms msArg') : mgssAcc)

            -- (t-apv) -----
            checkApp [tFun] es (mps : mgss) mgssAcc
             | Just (a', mArg) <- takeAnnMGTerm a mps
             = do (mArg',  tsResult, es') <- checkTermAppTerm  a wh ctx aFun tFun mArg
                  checkApp tsResult (es ++ es') mgss (MGAnn a' (MGTerm mArg') : mgssAcc)

            checkApp tsResult es [] mgssAcc
             = return (MAps mFun1 (reverse mgssAcc), tsResult, es)

            -- If the current function is multi valued and we still have arguments
            --   then we had a type abstraction that returned multiple values
            --   but haven't detected that when it was constructed.
            checkApp tsResult _ _ _
             = throw $ ErrorWrongArityUp UTerm a wh tsResult [TData]

        checkApp [tFun1] esFun1 mgss0 []

--------------------------------------------------------------------------------- App Term/Types --
-- | Check the application of a term to some types.
checkTermAppTypes
        :: Annot a => a -> [Where a]
        -> Context a -> a -> Type a -> [Type a]
        -> IO ([Type a], Type a)

checkTermAppTypes a wh ctx aFun tFun tsArg
 = do
        -- The funtion needs to have a forall type.
        tFun_red <- simplType a ctx tFun
        (bksParam, tResult)
         <- case tFun_red of
                TForall tps tResult
                  -> return (takeTPTypes tps, tResult)
                _ -> throw $ ErrorAppTermTypeCannot aFun wh tFun

        -- Check the kinds of the arguments.
        (tsArg', ksArg)
         <- checkTypes a wh ctx tsArg

        -- The number of arguments must match the number of parameters.
        when (not $ length bksParam == length tsArg)
         $ throw $ ErrorAppTermTypeWrongArity a wh bksParam tsArg

        -- Check the parameter and argument kinds match.
        (checkTypeEquivs ctx a [] (map snd bksParam) a [] ksArg
         >>= \case
                Nothing -> return ()
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                  -> throw $ ErrorMismatch UType a wh tErr1 tErr2)

        -- Substitute arguments into the result type to instantiate
        -- the type scheme.
        let nts    = [ (n, t) | (BindName n, _k) <- bksParam | t <- tsArg ]
        let snv    = snvOfBinds nts
        let tSubst = snvApplyType upsEmpty snv tResult

        -- Return the checked argument types and the instantiated scheme.
        return  (tsArg', tSubst)

--

---------------------------------------------------------------------------------- App Term/Term --
-- | Check the application of a functional term to an argument term.
--   Reduction of the argument may produce a vector of values.
checkTermAppTerm
        :: Annot a => a -> [Where a]
        -> Context a -> a -> Type a -> Term a
        -> IO (Term a, [Type a], [Effect a])

checkTermAppTerm a wh ctx aFun tFun mArg
 = do
        -- The function needs to have a functional type.
        tFun_red <- simplType a ctx tFun
        let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot aFun wh tFun)
                $ takeTFun tFun_red

        -- Check the types of the argument.
        (mArg', tsArg, esArg)
         <- checkTerm a wh ctx (Check tsParam) mArg

        -- The number of arguments must match the number of parameters.
        when (not $ length tsParam == length tsArg)
         $ throw $ ErrorAppTermTermWrongArity a wh tsParam tsArg

        -- Check the parameter and argument tpyes match.
        checkTypeEquivs ctx a [] tsParam a [] tsArg
         >>= \case
                Nothing -> return (mArg', tsResult, esArg)
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                 -> throw $ ErrorMismatch UType a wh tErr1 tErr2


--------------------------------------------------------------------------------- App Term/Terms --
-- | Check the application of a functional term to a vector of argument terms.
--   The arguments may only produce a single value each.
checkTermAppTerms
        :: Annot a => a -> [Where a]
        -> Context a -> a -> Type a -> [Term a]
        -> IO ([Term a], [Type a], [Effect a])

checkTermAppTerms a wh ctx aFun tFun msArg
 = do
        -- The function needs to have a functional type.
        tFun_red <- simplType a ctx tFun
        let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot aFun wh tFun)
                $ takeTFun tFun_red

        -- The number of arguments must match the number of parameters.
        when (not $ length msArg == length tsParam)
         $ throw $ ErrorAppTermTermWrongArityNum a wh tsParam (length msArg)

        -- Check the types of the arguments.
        (msArg', tsArg, esArgs)
         <- checkTerms a wh ctx (Check tsParam) msArg

        -- Check the parameter and argument types match.
        checkTypeEquivs ctx a [] tsParam a [] tsArg
         >>= \case
                Nothing -> return  (msArg', tsResult, esArgs)
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                 -> throw $ ErrorMismatch UType a wh tErr1 tErr2



