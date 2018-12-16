
module Salt.Core.Check.Term where
import Salt.Core.Check.Eq
import Salt.Core.Check.Reduce
import Salt.Core.Check.Type
import Salt.Core.Check.Kind
import Salt.Core.Check.Context
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Transform.MapAnnot
import Salt.Core.Transform.Subst
import Salt.Core.Exp
import Salt.Core.Codec.Text             ()
import qualified Salt.Core.Prim.Ops     as Prim
import qualified Salt.Core.Prim.Ctor    as Prim
import qualified Salt.Data.List         as List
import qualified Salt.Data.Pretty       as P

import Control.Exception
import Control.Monad
import Data.Maybe
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Text.Show.Pretty       as Text


---------------------------------------------------------------------------------------------------
-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
checkTerm
        :: Annot a => a -> [Where a] -> Context a
        -> Term a -> Mode a
        -> IO (Term a, [Type a], [Effect a])

-- (t-ann) ------------------------------------------------
checkTerm _a wh ctx (MAnn a' m) mode
 = checkTerm a' wh ctx m mode


-- (t-mmm) ------------------------------------------------
checkTerm a wh ctx (MTerms msArg) mode
 = do   (msArg', tsArg, esArg)
         <- checkTerms a wh ctx msArg mode
        return  (MTerms msArg', tsArg, esArg)


-- (t-the) ------------------------------------------------
checkTerm a wh ctx (MThe ts m) Synth
 = do   (m', _, es) <- checkTerm a wh ctx m (Check ts)
        return  (MThe ts m', ts, es)


-- (t-box) ------------------------------------------------
checkTerm a wh ctx (MBox m) Synth
 = do   (m', ts, es) <- checkTerm a wh ctx m Synth
        let tEff = flattenType (TSum es)
        return  (MBox m', [TSusp ts tEff], [])


-- (t-run) ------------------------------------------------
checkTerm a wh ctx (MRun m) Synth
 = do
        -- Check the body.
        (m', tsSusp', es) <- checkTerm a wh ctx m Synth

        -- The body must produce a suspension.
        -- When we run it it causes the effects in its annotations.
        tsSusp_red'       <- reduceTypes a wh ctx tsSusp'
        case tsSusp_red' of
         [TSusp tsResult' e']
            -> return (MRun m', tsResult', es ++ [e'])
         _  -> throw $ ErrorRunSuspensionIsNot a wh tsSusp_red'


-- (t-val) ------------------------------------------------
checkTerm a wh ctx m@(MRef (MRVal v)) Synth
 = do   t <- checkValue a wh ctx v
        return (m, [t], [])


-- (t-prm) ------------------------------------------------
checkTerm a wh ctx m@(MRef (MRPrm nPrim)) Synth
 | Just pp <- Map.lookup nPrim Prim.primOps
 = do   let tPrim = mapAnnot (const a) $ Prim.typeOfPrim pp

        pss <- stripTermParamsOfType ctx tPrim
        when (not $ null pss)
         $ throw $ ErrorUnsaturatedPrim a wh nPrim tPrim

        return (m, [tPrim], [])

 | Just t <- Map.lookup nPrim Prim.primDataCtors
 = do   let tCon = mapAnnot (const a) t

        pss <- stripTermParamsOfType ctx tCon
        when (not $ null pss)
         $ throw $ ErrorUnsaturatedCtor a wh nPrim tCon

        return (m, [tCon], [])

 | otherwise
 = throw $ ErrorUnknownPrimitive a wh nPrim


-- (t-con) ------------------------------------------------
checkTerm a wh ctx m@(MRef (MRCon nCon)) Synth
 =  contextResolveDataCtor nCon ctx
 >>= \case
        Nothing
         -> throw $ ErrorUnknownDataCtor a wh nCon

        Just tCtor
         -> do  let tCtor' = mapAnnot (const a) tCtor
                return (m, [tCtor'], [])


-- (t-var) ------------------------------------------------
checkTerm a wh ctx m@(MVar u) Synth
 = do   mt <- contextResolveTermBound u ctx
        case mt of
         Just t  -> return (m, [t], [])
         Nothing -> throw $ ErrorUnknownTermBound a wh u


-- (t-abt) ------------------------------------------------
checkTerm a wh ctx (MAbs ps@MPTypes{} m) Synth
 = do
        -- Check the parameters and bind into the context.
        ps'@(MPTypes bts) <- checkTermParams a wh ctx ps
        let ctx'    =  contextBindTermParams ps ctx

        -- Check the body of the abstraction in the new context.
        (m', t, es) <- checkTerm1 a wh ctx' m Synth

        -- The body must be pure.
        eBody_red   <- reduceType a wh ctx' (TSum es)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorImpureTypeAbstraction a wh eBody_red

        return  (MAbs ps' m', [TForall bts t], [])


-- (t-abm) ------------------------------------------------
checkTerm a wh ctx (MAbs ps@MPTerms{} m) Synth
 = do
        -- Check the parameters and bind them into the context.
        ps'@(MPTerms bts) <- checkTermParams a wh ctx ps
        let ctx' =  contextBindTermParams ps ctx

        -- Check the body of the abstraction in the new context.
        (m', ts, es) <- checkTerm a wh ctx' m Synth

        -- The body must be pure.
        eBody_red    <- reduceType a wh ctx' (TSum es)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorImpureTermAbstraction a wh eBody_red

        return  (MAbs ps' m', [TFun (map snd bts) ts], [])


-- (t-aps) ------------------------------------------------
-- This handles (t-apt), (t-apm) and (t-apv) from the docs.
checkTerm a wh ctx (MAps mFun0 mgss0) Synth
 = do
        -- If this an effectful primitive then also add the effects
        -- we get from applying it.
        (mFun1, tFun1, esFun1)
         <- case takeMPrm mFun0 of
                Just nPrm
                 | Just pp      <- Map.lookup nPrm Prim.primOps
                 -> do  let tPrim   = mapAnnot (const a) $ Prim.typeOfPrim pp
                        let ePrim   = mapAnnot (const a) $ Prim.effectOfPrim pp

                        pss     <- stripTermParamsOfType ctx tPrim
                        when (length pss /= length mgss0)
                         $ throw $ ErrorUnsaturatedPrim a wh nPrm tPrim

                        return (mFun0, tPrim, [ePrim])

                 | Just tCon    <- Map.lookup nPrm Prim.primDataCtors
                 -> do  let tCon'   = mapAnnot (const a) tCon

                        pss     <- stripTermParamsOfType ctx tCon'
                        when (length pss /= length mgss0)
                         $ throw $ ErrorUnsaturatedCtor a wh nPrm tCon'

                        return (mFun0, tCon', [])

                 | otherwise    -> throw $ ErrorUnknownPrimitive a wh nPrm

                Nothing
                 -> checkTerm1 a wh ctx mFun0 Synth

        -- Check that we have at least some arguments to apply.
        when (null mgss0)
         $ throw $ ErrorAppNoArguments a wh tFun1

        -- Check argument types line up with parameter types.
        let -- (t-apt) -----
            checkApp [tFun] es (MGTypes tsArg : mgss) mgssAcc
             = do (tsArg', tResult)  <- checkTermAppTypes a wh ctx tFun tsArg
                  checkApp [tResult] es mgss (MGTypes tsArg' : mgssAcc)

            -- (t-apm) -----
            checkApp [tFun] es (MGTerms msArg : mgss) mgssAcc
             = do (msArg', tsResult, es') <- checkTermAppTerms a wh ctx tFun msArg
                  checkApp tsResult (es ++ es') mgss (MGTerms msArg' : mgssAcc)

            -- (t-apv) -----
            checkApp [tFun] es (MGTerm  mArg  : mgss) mgssAcc
             = do (mArg',  tsResult, es') <- checkTermAppTerm  a wh ctx tFun mArg
                  checkApp tsResult (es ++ es') mgss (MGTerm  mArg'  : mgssAcc)

            checkApp tsResult es [] mgssAcc
             = return (MAps mFun1 (reverse mgssAcc), tsResult, es)

            -- If the current function is multi valued and we still have arguments
            --   then we had a type abstraction that returned multiple values
            --   but haven't detected that when it was constructed.
            checkApp tsResult _ _ _
             = throw $ ErrorTermsWrongArity a wh tsResult [TData]

        checkApp [tFun1] esFun1 mgss0 []


-- (t-let) ------------------------------------------------
checkTerm a wh ctx (MLet bts mBind mBody) Synth
 = do
        -- Check kinds of binder annotations.
        MPTerms bts'
         <- checkTermParams a wh ctx (MPTerms bts)

        -- Check the bound expression.
        (mBind', tsBind, esBind)
         <- checkTerm a wh ctx mBind Synth

        -- Check we have the same number of binders
        -- as values produced by the binding.
        let (bs, tsParam) = unzip bts'
        when (not $ length tsParam == length tsBind)
         $ throw $ ErrorLetWrongArity a wh tsBind bs

        -- Check binding types against any annotations for them,
        -- then add them to the context.
        let checkLetAnnot tAnnot tBind
             | THole    <- tAnnot
             = return tBind

             | otherwise
             = case checkTypeEq a [] tAnnot a [] tBind of
                 Nothing        -> return tBind
                 Just ((_a1, tErr1), (_a2, tErr2))
                  -> throw $ ErrorTypeMismatch a wh tErr1 tErr2

        tsBind'   <- zipWithM checkLetAnnot tsParam tsBind
        let bts'' = zip bs tsBind'
        let ctx' = contextBindTermParams (MPTerms bts'') ctx

        -- Check the body term.
        (mBody', tsResult, esResult)
         <- checkTerm a wh ctx' mBody Synth

        return  ( MLet bts' mBind' mBody'
                , tsResult
                , esBind ++ esResult)


-- (t-rec) ------------------------------------------------
checkTerm a wh ctx mm@(MRecord ns ms) mode
 = case mode of
        -- If we have an expected type for all the fields then check the fields
        -- separately. This gives better error messages as we don't need to report
        -- types for fields that are irrelevant to the problem.
        Check [TRecord ns' tgsExpected]
         | Set.fromList ns == Set.fromList ns'
         , Set.size (Set.fromList ns)  == length ns
         , Set.size (Set.fromList ns') == length ns'
         -> do
                -- Check each of the field terms against the expected type for it.
                let nts   = Map.fromList $ zip ns' tgsExpected
                let nmtgs = [ let Just tg = Map.lookup n nts in (n, m, tg)
                            | m  <- ms | n <- ns ]

                -- When we do this we set the 'where' to the specific field we
                -- are checking, which makes the error messages easier to read.
                (ms', tss', ess')
                  <- fmap unzip3 $ forM nmtgs $ \(n, m, (TGTypes ts))
                  -> do let wh' = WhereRecordField a n (Just ts) : wh
                        checkTerm a wh' ctx m (Check ts)

                return  ( MRecord ns ms'
                        , [TRecord ns $ map TGTypes tss']
                        , concat ess')

        -- The expected type we have doesn't cover all the fields of the record
        -- being constructed. We call the default checker and let that fail.
        Check _
         -> checkTerm_default a wh ctx mm mode

        -- Synthesise a type for the record.
        Synth
         -> do  -- Check for duplicate fields.
                let nsDup = List.duplicates ns
                when (not $ null nsDup)
                 $ throw $ ErrorRecordDuplicateFields a wh nsDup

                -- Check each of the field terms.
                (ms', tss', ess')
                 <- fmap unzip3
                  $ mapM (\m -> checkTerm a wh ctx m Synth) ms

                return  ( MRecord ns ms'
                        , [TRecord ns (map TGTypes tss')]
                        , concat ess')


-- (t-prj) ------------------------------------------------
checkTerm a wh ctx (MProject nLabel mRecord) Synth
 = do
        -- Check the body expression.
        (mRecord', tsRecord, esRecord)
         <- checkTerm a wh ctx mRecord Synth

        -- The body needs to have record type with the field that we were expecting.
        tsRecord' <- reduceTypes a wh ctx tsRecord
        case tsRecord' of
         [tRecord@(TRecord ns tgs)]
          -> case lookup nLabel $ zip ns tgs of
                Nothing
                 -> throw $ ErrorRecordProjectNoField a wh tRecord nLabel

                Just (TGTypes tsField)
                 -> return (MProject nLabel mRecord', tsField, esRecord)

         [tThing]
            -> throw $ ErrorRecordProjectIsNot a wh tThing nLabel

         ts -> throw $ ErrorTermsWrongArity a wh ts [TData]


-- (t-vnt) ------------------------------------------------
checkTerm a wh ctx (MVariant nLabel msValues tVariant) Synth
 = do
        -- The annotation tells us what type to expect for the body.
        tVariant'
         <- reduceType a wh ctx tVariant

        tsExpected'
         <- case tVariant' of
                TVariant ns tgs
                 -> case lookup nLabel $ zip ns tgs of
                        Nothing
                         -> throw $ ErrorVariantAnnotAltMissing a wh tVariant' nLabel
                        Just (TGTypes ts)
                         -> return ts
                _ -> throw $ ErrorVariantAnnotIsNot a wh tVariant'

        -- Check the body against the type from the annotation.
        (msValues', _tsValues, esValues)
         <- checkTerms a wh ctx msValues (Check tsExpected')

        return  ( MVariant nLabel msValues' tVariant
                , [tVariant], esValues)


-- (t-cse) ------------------------------------------------
checkTerm a wh ctx (MCase mScrut ls msAlt) Synth
 = do   (mScrut', _tScrut, esScrut)
         <- checkTerm1 a wh ctx mScrut Synth

        (msAlt',  tsAlt, esAltComp)
         <- fmap unzip3 $ mapM (\m -> checkTerm1 a wh ctx m Synth) msAlt

        -- TODO: check alts all have same result type.
        -- TODO: check scrut matches alt head.
        -- TODO: combine result effects of all alts.
        let (TFun _ tsResult : _) = tsAlt
        return  ( MCase mScrut' ls msAlt'
                , tsResult
                , esScrut ++ concat esAltComp)


-- (t-ifs) ------------------------------------------------
checkTerm a wh ctx (MIf msCond msThen mElse) Synth
 = do   (msCond', _tssCond, esCond)
         <- checkTerms a wh ctx msCond
         $  Check $ replicate (length msCond) TBool

        (msThen', _tssThen, esThen)
         <- checkTerms a wh ctx msThen Synth

        (mElse',  tsElse, esElse)
         <- checkTerm  a wh ctx mElse  Synth

        -- TODO: check tsThen and tsElse matches
        -- TODO: check there are the same number of conds and then exps.
        return  ( MIf msCond' msThen' mElse'
                , tsElse
                , esCond ++ esThen ++ esElse)


-- (t-lst) ------------------------------------------------
-- TODO: check embedded type.
checkTerm a wh ctx (MList t ms) Synth
 = do   let ts = replicate (length ms) t
        (ms', _, es) <- checkTerms a wh ctx ms (Check ts)
        return  (MList t ms', [TList t], es)


-- (t-set) ------------------------------------------------
-- TODO: check embedded type.
checkTerm a wh ctx (MSet t ms) Synth
 = do   let ts = replicate (length ms) t
        (ms', _, es) <- checkTerms a wh ctx ms (Check ts)
        return (MSet t ms', [TSet t], es)


-- (t-map) ------------------------------------------------
-- TODO: check embedded types.
-- TODO: check keys and values same length.
checkTerm a wh ctx (MMap tk tv msk msv) Synth
 = do   let tsk = replicate (length msk) tk
        let tsv = replicate (length msv) tv
        (msk', _, esKeys) <- checkTerms a wh ctx msk (Check tsk)
        (msv', _, esVals) <- checkTerms a wh ctx msv (Check tsv)
        return  ( MMap tk tv msk' msv'
                , [TMap tk tv]
                , esKeys ++ esVals)

checkTerm a wh ctx m mode
 = checkTerm_default a wh ctx m mode


-----------------------------------------------------------
checkTerm_default a wh ctx m (Check tsExpected)
 = do   (m', tsActual, esActual)
         <- checkTerm a wh ctx m Synth

        when (length tsActual /= length tsExpected)
         $ throw $ ErrorTermsWrongArity a wh tsActual tsExpected

        case checkTypeEqs a [] tsExpected a [] tsActual of
         Nothing -> return (m', tsActual, esActual)
         Just ((_a1, t1Err), (_a2, t2Err))
          -> throw $ ErrorTypeMismatch a wh t1Err t2Err


checkTerm_default _ _ _ mm mode
 =  error $ unlines
        [ "checkTerm: not finished"
        , Text.ppShow mode
        , Text.ppShow mm
        , P.renderIndent $ P.ppr () mm ]


-- (t-one) ----------------------------------------------------------------------------------------
-- | Like 'checkTerm' but expect a single result type.
checkTerm1
        :: Annot a => a -> [Where a]
        -> Context a -> Term a -> Mode a
        -> IO (Term a, Type a, [Effect a])

checkTerm1 a wh ctx m mode
 = do   (m', ts', es')
         <- checkTerm a wh ctx m mode

        case ts' of
         [t]    -> return (m', t, es')
         _      -> throw $ ErrorTermsWrongArity a wh ts' [TData]


-- (t-many / t-gets) ------------------------------------------------------------------------------
-- This function implements both the t-many and t-gets rules from the declarative
-- version of the typing rules. In this algorithmic, bidirectional implementation
-- the expected types are represented in the checker mode we get from the caller.

-- | Check a list of individual terms.
checkTerms
        :: Annot a => a -> [Where a]
        -> Context a -> [Term a] -> Mode a
        -> IO ([Term a], [Type a], [Effect a])

checkTerms a wh ctx ms Synth
 = do   (ms', ts', ess')
         <- fmap unzip3 $ mapM     (\m -> checkTerm1 a wh ctx m Synth) ms
        return (ms', ts', concat ess')

checkTerms a wh ctx ms (Check ts)
 | length ms == length ts
 = do   (ms', ts', ess')
         <- fmap unzip3 $ zipWithM (\m t -> checkTerm1 a wh ctx m (Check [t])) ms ts
        return (ms', ts', concat ess')

 | otherwise
 = do   (_ms, ts', _ess')
         <- fmap unzip3 $ mapM (\m -> checkTerm1 a wh ctx m Synth) ms
        throw $ ErrorTermsWrongArity a wh ts' (replicate (length ms) TData)


-- | Check the given terms all have the specified type,
--   bundling all the caused effects together in the result.
checkTermsAreAll
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Term a]
        -> IO ([Term a], [Effect a])

checkTermsAreAll a wh ctx tExpected ms
 = do   (ms', _ts, effs)
         <- fmap unzip3
          $ mapM (\t -> checkTerm1 a wh ctx t (Check [tExpected])) ms

        return (ms', concat effs)


---------------------------------------------------------------------------------------------------
-- | Check some term function parameters.
checkTermParams
        :: Annot a => a -> [Where a]
        -> Context a -> TermParams a -> IO (TermParams a)

checkTermParams a wh ctx mps
 = case mps of
        MPTerms bts
         -> do  let (bs, ts) = unzip bts
                ts' <- checkTypesAre a wh ctx ts (replicate (length ts) TData)
                return  $ MPTerms $ zip bs ts'

        MPTypes bks
         -> do  let (bs, ks) = unzip bks
                ks' <- mapM (checkKind a wh ctx) ks
                return  $ MPTypes $ zip bs ks'


-- | Check a list of term function parameters,
--   where type variables bound earlier in the list are in scope
--   when checking types annotating term variables later in the list.
checkTermParamss
        :: Annot a => a -> [Where a]
        -> Context a -> [TermParams a] -> IO [TermParams a]

checkTermParamss _a _wh _ctx []
 = return []

checkTermParamss a wh ctx (tps : tpss)
 = do   tps'  <- checkTermParams  a wh ctx  tps
        let ctx'  = contextBindTermParams tps' ctx
        tpss' <- checkTermParamss a wh ctx' tpss
        return $ tps' : tpss'


---------------------------------------------------------------------------------------------------
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
        (tsArg', ksArg) <- checkTypes a wh ctx tsArg

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
         <- checkTerm a wh ctx mArg (Check tsParam)

        -- The number of arguments must match the number of parameters.
        when (not $ length tsParam == length tsArg)
         $ throw $ ErrorAppTermTermWrongArity a wh tsParam tsArg

        -- Check the parameter and argument tpyes match.
        case checkTypeEqs a [] tsParam a [] tsArg of
         Just ((_aErr1', tErr1), (_aErr2', tErr2))
           -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
         _ -> return (mArg', tsResult, esArg)


-- | Check the application of a functional term to some argument terms.
--   The arguments can only produce a single value each.
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
         <- checkTerms a wh ctx msArg (Check tsParam)

        -- Check the parameter and argument types match.
        case checkTypeEqs a [] tsParam a [] tsArg of
         Just ((_aErr1', tErr1), (_aErr2', tErr2))
           -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
         _ -> return  (msArg', tsResult, esArgs)


---------------------------------------------------------------------------------------------------
-- | Check a value, yielding its type.
checkValue
        :: Annot a => a -> [Where a]
        -> Context a -> Value a -> IO (Type a)

checkValue a wh ctx v
 = case v of
        VUnit     -> return TUnit
        VSymbol{} -> return TSymbol
        VText{}   -> return TText
        VBool{}   -> return TBool
        VInt{}    -> return TInt
        VNat{}    -> return TNat
        VNone t   -> return $ TOption t

        VData n ts vs
         -> do  -- Use the term checker to check the applications.
                (_m, tResult, [])
                 <- checkTerm1 a wh ctx
                        (MApm (MApt (MCon n) ts) (map MVal vs)) Synth
                return tResult

        VRecord nvs
         -> do  let (ns, vss) = unzip nvs
                tss <- mapM (mapM (checkValue a wh ctx)) vss
                return (TRecord ns $ map TGTypes tss)

        VVariant _n tVar vs
         -> do  checkType a wh ctx tVar
                _ts <- mapM (checkValue a wh ctx) vs
                -- TODO: check embedded type.
                return tVar

        VList t vs
         -> do  checkType a wh ctx t
                checkValuesAreAll a wh ctx t vs
                return $ TList t

        VSet t vsSet
         -> do  checkType a wh ctx t
                checkValuesAreAll a wh ctx t  $ map (mapAnnot (const a)) $ Set.toList vsSet
                return $ TSet t

        VMap tk tv vsMap
         -> do  checkType a wh ctx tk
                checkType a wh ctx tv
                checkValuesAreAll a wh ctx tk $ map (mapAnnot (const a)) $ Map.keys vsMap
                checkValuesAreAll a wh ctx tv $ Map.elems vsMap
                return $ TMap tk tv

        VClosure (Closure env mps mBody)
         -> do  -- Build a context with just the closure environment.
                ctx1    <- contextBindEnv a wh env
                        $  Context
                        {  contextModuleTerm = contextModuleTerm ctx
                        ,  contextLocal      = []}

                -- Bind the closure parameters into the context.
                MPTerms btsParam <- checkTermParams a wh ctx mps
                let tsParam     = map snd btsParam
                let ctx2        = contextBindTermParams mps ctx1

                -- Check the body expression.
                (_, tsResult, esBody) <- checkTerm a wh ctx2 mBody Synth

                -- The body must be pure.
                eBody_red    <- reduceType a wh ctx2 (TSum esBody)
                when (not $ isTPure eBody_red)
                 $ throw $ ErrorImpureTermAbstraction a wh eBody_red

                return $ TFun tsParam tsResult


-- | Check that a value has the given type.
checkValueIs
        :: Annot a => a -> [Where a] -> Context a
        -> Type a -> Value a -> IO ()

checkValueIs a wh ctx tExpected v
 = do   tActual <- checkValue a wh ctx v

        case checkTypeEqs a [] [tExpected] a [] [tActual] of
         Nothing -> return ()
         Just ((_a1, t1Err), (_a2, t2Err))
          -> throw $ ErrorTypeMismatch a wh t1Err t2Err


-- | Check that a list of values all have the given type.
checkValuesAreAll
        :: Annot a => a -> [Where a] -> Context a
        -> Type a -> [Value a] -> IO ()

checkValuesAreAll a wh ctx t vs
 = mapM_ (checkValueIs a wh ctx t) vs


-- | Check an environment binding and add it to the context.
contextBindEnv
        :: Annot a
        => a -> [Where a] -> Env a
        -> Context a -> IO (Context a)

contextBindEnv a wh (Env bs0) ctx0
 = go ctx0 (reverse bs0)
 where
        go ctx (EnvType n t : bs)
         = do   (_, k') <- checkType a wh ctx t
                go (contextBindType n k' ctx) bs

        go ctx (EnvValue n v : bs)
         = do   (_, [t'], _) <- checkTerm a wh ctx (MVal v) Synth
                go (contextBindTerm n t' ctx) bs

        go ctx []
         = return $ ctx

