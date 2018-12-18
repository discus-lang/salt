
module Salt.Core.Check.Term where
import Salt.Core.Check.Term.App
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Value
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import Salt.Core.Codec.Text             ()
import qualified Salt.Core.Prim.Ops     as Prim
import qualified Salt.Core.Prim.Ctor    as Prim
import qualified Salt.Data.List         as List
import qualified Salt.Data.Pretty       as P
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Text.Show.Pretty       as Text


-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
checkTermWith :: CheckTerm a

-- (t-ann) ------------------------------------------------
checkTermWith _a wh ctx mode (MAnn a' m)
 = checkTerm a' wh ctx mode m


-- (t-mmm) ------------------------------------------------
checkTermWith a wh ctx mode (MTerms msArg)
 = do   (msArg', tsArg, esArg)
         <- checkTerms a wh ctx mode msArg
        return  (MTerms msArg', tsArg, esArg)


-- (t-the) ------------------------------------------------
checkTermWith a wh ctx Synth (MThe ts m)
 = do   (m', _, es) <- checkTerm a wh ctx (Check ts) m
        return  (MThe ts m', ts, es)


-- (t-box) ------------------------------------------------
checkTermWith a wh ctx Synth (MBox m)
 = do   (m', ts, es) <- checkTerm a wh ctx Synth m
        let tEff = flattenType (TSum es)
        return  (MBox m', [TSusp ts tEff], [])


-- (t-run) ------------------------------------------------
checkTermWith a wh ctx Synth (MRun m)
 = do
        -- Check the body.
        (m', tsSusp', es)
         <- checkTerm a wh ctx Synth m

        -- The body must produce a suspension.
        -- When we run it it causes the effects in its annotations.
        tsSusp_red' <- reduceTypes a wh ctx tsSusp'
        case tsSusp_red' of
         [TSusp tsResult' e']
            -> return (MRun m', tsResult', es ++ [e'])
         _  -> throw $ ErrorRunSuspensionIsNot a wh tsSusp_red'


-- (t-val) ------------------------------------------------
checkTermWith a wh ctx Synth m@(MRef (MRVal v))
 = do   t <- checkValue a wh ctx v
        return (m, [t], [])


-- (t-prm) ------------------------------------------------
checkTermWith a wh ctx Synth m@(MRef (MRPrm nPrim))
 | Just pp <- Map.lookup nPrim Prim.primOps
 = do
        let tPrim = mapAnnot (const a) $ Prim.typeOfPrim pp
        pss <- stripTermParamsOfType ctx tPrim
        when (not $ null pss)
         $ throw $ ErrorUnsaturatedPrim a wh nPrim tPrim

        return (m, [tPrim], [])

 | Just t <- Map.lookup nPrim Prim.primDataCtors
 = do
        let tCon = mapAnnot (const a) t
        pss <- stripTermParamsOfType ctx tCon
        when (not $ null pss)
         $ throw $ ErrorUnsaturatedCtor a wh nPrim tCon

        return (m, [tCon], [])

 | otherwise
 = throw $ ErrorUnknownPrimitive a wh nPrim


-- (t-con) ------------------------------------------------
checkTermWith a wh ctx Synth m@(MRef (MRCon nCon))
 =  contextResolveDataCtor nCon ctx
 >>= \case
        Nothing
         -> throw $ ErrorUnknownDataCtor a wh nCon

        Just tCtor
         -> do  let tCtor' = mapAnnot (const a) tCtor
                return (m, [tCtor'], [])


-- (t-var) ------------------------------------------------
checkTermWith a wh ctx Synth m@(MVar u)
 =   contextResolveTermBound u ctx
 >>= \case
         Just t  -> return (m, [t], [])
         Nothing -> throw $ ErrorUnknownTermBound a wh u


-- (t-abt) ------------------------------------------------
checkTermWith a wh ctx Synth (MAbs ps@MPTypes{} m)
 = do
        -- Check the parameters and bind into the context.
        ps'@(MPTypes bts) <- checkTermParams a wh ctx ps
        let ctx' = contextBindTermParams ps ctx

        -- Check the body of the abstraction in the new context.
        (m', t, es) <- checkTerm1 a wh ctx' Synth m

        -- The body must be pure.
        eBody_red   <- reduceType a wh ctx' (TSum es)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorImpureTypeAbstraction a wh eBody_red

        return  (MAbs ps' m', [TForall bts t], [])


-- (t-abm) ------------------------------------------------
checkTermWith a wh ctx Synth (MAbs ps@MPTerms{} m)
 = do
        -- Check the parameters and bind them into the context.
        ps'@(MPTerms bts) <- checkTermParams a wh ctx ps
        let ctx' =  contextBindTermParams ps ctx

        -- Check the body of the abstraction in the new context.
        (m', ts, es) <- checkTerm a wh ctx' Synth m

        -- The body must be pure.
        eBody_red    <- reduceType a wh ctx' (TSum es)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorImpureTermAbstraction a wh eBody_red

        return  (MAbs ps' m', [TFun (map snd bts) ts], [])


-- (t-aps) ------------------------------------------------
-- This handles (t-apt), (t-apm) and (t-apv) from the docs.
checkTermWith a wh ctx Synth (MAps mFun0 mgss0)
 = do
        -- If this an effectful primitive then also add the effects
        -- we get from applying it.
        (mFun1, tFun1, esFun1)
         <- case takeMPrm mFun0 of
                Just nPrm
                 | Just pp  <- Map.lookup nPrm Prim.primOps
                 -> do
                        let tPrim = mapAnnot (const a) $ Prim.typeOfPrim pp
                        let ePrim = mapAnnot (const a) $ Prim.effectOfPrim pp

                        pss     <- stripTermParamsOfType ctx tPrim
                        when (length pss /= length mgss0)
                         $ throw $ ErrorUnsaturatedPrim a wh nPrm tPrim

                        return (mFun0, tPrim, [ePrim])

                 | Just tCon <- Map.lookup nPrm Prim.primDataCtors
                 -> do
                        let tCon'   = mapAnnot (const a) tCon

                        pss <- stripTermParamsOfType ctx tCon'
                        when (length pss /= length mgss0)
                         $ throw $ ErrorUnsaturatedCtor a wh nPrm tCon'

                        return (mFun0, tCon', [])

                 | otherwise    -> throw $ ErrorUnknownPrimitive a wh nPrm

                Nothing
                 -> checkTerm1 a wh ctx Synth mFun0

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
checkTermWith a wh ctx Synth (MLet bts mBind mBody)
 = do
        -- Check kinds of binder annotations.
        MPTerms bts'
         <- checkTermParams a wh ctx (MPTerms bts)

        -- Check the bound expression.
        (mBind', tsBind, esBind)
         <- checkTerm a wh ctx Synth mBind

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
        let ctx'  = contextBindTermParams (MPTerms bts'') ctx

        -- Check the body term.
        (mBody', tsResult, esResult)
         <- checkTerm a wh ctx' Synth mBody

        return  ( MLet bts' mBind' mBody'
                , tsResult
                , esBind ++ esResult)


-- (t-rec) ------------------------------------------------
checkTermWith a wh ctx mode mm@(MRecord ns ms)
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
                        checkTerm a wh' ctx (Check ts) m

                return  ( MRecord ns ms'
                        , [TRecord ns $ map TGTypes tss']
                        , concat ess')

        -- The expected type we have doesn't cover all the fields of the record
        -- being constructed. We call the default checker and let that fail.
        Check tsExpected
         -> checkTermIs a wh ctx tsExpected mm

        -- Synthesise a type for the record.
        Synth
         -> do  -- Check for duplicate fields.
                let nsDup = List.duplicates ns
                when (not $ null nsDup)
                 $ throw $ ErrorRecordDuplicateFields a wh nsDup

                -- Check each of the field terms.
                (ms', tss', ess')
                 <- fmap unzip3 $ mapM (checkTerm a wh ctx Synth) ms

                return  ( MRecord ns ms'
                        , [TRecord ns (map TGTypes tss')]
                        , concat ess')


-- (t-prj) ------------------------------------------------
checkTermWith a wh ctx Synth (MProject nLabel mRecord)
 = do
        -- Check the body expression.
        (mRecord', tRecord, esRecord)
         <- checkTerm1 a wh ctx Synth mRecord

        -- The body needs to have record type with the field that we were expecting.
        (ns, tgs, tRecord')
         <- reduceType a wh ctx tRecord
         >>= \case
                t@(TRecord ns tgs) -> return (ns, tgs, t)
                tThing  -> throw $ ErrorRecordProjectIsNot a wh tThing nLabel

        -- Lookup the types of the field.
        tsField
         <- case lookup nLabel $ zip ns tgs of
                Just (TGTypes tsField) -> return tsField
                Nothing -> throw $ ErrorRecordProjectNoField a wh tRecord' nLabel

        return  ( MProject nLabel mRecord'
                , tsField, esRecord)


-- (t-vnt) ------------------------------------------------
checkTermWith a wh ctx Synth (MVariant nLabel msValues tVariant)
 = do
        -- Check annotation is well kinded.
        checkType a wh ctx tVariant

        -- The annotation tells us what type to expect for the body.
        (ns, tgs, tVariant')
         <- reduceType a wh ctx tVariant
         >>= \case
                t@(TVariant ns tgs) -> return (ns, tgs, t)
                tThing -> throw $ ErrorVariantAnnotIsNot a wh tThing

        -- Lookup the types of the alternative.
        tsExpected'
         <- case lookup nLabel $ zip ns tgs of
                Just (TGTypes ts) -> return ts
                _ -> throw $ ErrorVariantAnnotAltMissing a wh tVariant' nLabel

        -- Check the body against the type from the annotation.
        (msValues', _tsValues, esValues)
         <- checkTerms a wh ctx (Check tsExpected') msValues

        return  ( MVariant nLabel msValues' tVariant
                , [tVariant], esValues)


-- (t-cse) ------------------------------------------------
checkTermWith a wh ctx Synth mCase@(MVarCase mScrut msAlt)
 = do
        -- Check the scrutinee.
        (mScrut', tScrut, esScrut)
         <- checkTerm1 a wh ctx Synth mScrut

        -- The scrutinee needs to be a variant.
        (nsScrut, mgsScrut)
         <- reduceType a wh ctx tScrut
         >>= \case
                TVariant ns mgs -> return (ns, mgs)
                _ -> throw $ ErrorCaseScrutNotVariant a wh tScrut

        -- Check for overlapping alternatives.
        let nsAlt = [n | MVarAlt n _ _ <- msAlt]
        let nsDup = List.duplicates nsAlt
        when (not $ null nsDup)
         $ throw $ ErrorCaseAltsOverlapping a wh nsDup

        -- Check for inexhaustive alternatives.
        let nsNot = Set.difference (Set.fromList nsScrut) (Set.fromList nsAlt)
        when (not $ Set.null nsNot)
         $ throw $ ErrorCaseAltsInexhaustive a wh (Set.toList nsNot) tScrut

        -- Check all alternatives in turn,
        --  collecting up all the effects,
        --  and ensuring all the alt result types match.
        let nmgsScrut = zip nsScrut mgsScrut
        let checkAlts (MVarAlt n btsPat mBody : msAltsRest)
                      msAltsChecked mtsResult esAlt
             = do
                  -- Lookup the field types from the type of the scrutinee.
                  -- The type of the scrutinee must cover this alternative.
                  tsField
                   <- case lookup n nmgsScrut of
                        Just (TGTypes ts) -> return ts
                        Nothing -> throw $ ErrorCaseAltNotInVariant a wh n tScrut

                  -- Check the pattern field types match the fields of the scrutinee.
                  let tsPat = map snd btsPat
                  (case checkTypeEqs a [] tsPat a [] tsField of
                        Nothing -> return ()
                        Just ((_a1, t1), (_a2, t2))
                         -> throw $ ErrorCaseAltPatMismatch a wh n t1 t2)

                  -- Check the result in the context extended by the fields
                  -- we matched with the pattern. Also ensure this alternative
                  -- has the same result type as any others we have checked before.
                  let ctx'  = contextBindTermParams (MPTerms btsPat) ctx
                  let mode' = case mtsResult of
                                Nothing -> Synth
                                Just ts -> Check ts

                  (mBody', tsResult, esResult)
                   <- checkTerm a wh ctx' mode' mBody

                  checkAlts
                        msAltsRest
                        (MVarAlt n btsPat mBody' : msAltsChecked)
                        (Just tsResult)
                        (esResult ++ esAlt)

            checkAlts [] msAltsChecked (Just tsResult) esAlt
                = return ( reverse msAltsChecked
                         , tsResult
                         , reverse esAlt)

            -- There are either no alternatives or one of them is
            -- not a MVarAlt term.
            checkAlts _ _ _ _
             = throw $ ErrorTermMalformed a wh mCase

        (msAlt', tsResult, esResult)
         <- checkAlts msAlt [] Nothing []

        return  ( MVarCase mScrut' msAlt'
                , tsResult
                , esScrut ++ esResult)


-- (t-ifs) ------------------------------------------------
checkTermWith a wh ctx Synth mIf@(MIf msCond msThen mElse)
 = do
        when (not $ length msCond == length msThen)
         $ throw $ ErrorTermMalformed a wh mIf

        (msCond', esCond)
         <- checkTermsAreAll a wh ctx TBool msCond

        (mElse',  tsElse, esElse)
         <- checkTerm a wh ctx Synth mElse

        (msThen', _tssThen, essThen)
         <- fmap unzip3
         $  mapM (checkTerm a wh ctx (Check tsElse)) msThen

        return  ( MIf msCond' msThen' mElse'
                , tsElse
                , esCond ++ concat essThen ++ esElse)


-- (t-lst) ------------------------------------------------
checkTermWith a wh ctx Synth (MList t ms)
 = do   t' <- checkTypeIs a wh ctx TData t
        (ms', es) <- checkTermsAreAll a wh ctx t' ms
        return  (MList t' ms', [TList t], es)


-- (t-set) ------------------------------------------------
checkTermWith a wh ctx Synth (MSet t ms)
 = do   t' <- checkTypeIs a wh ctx TData t
        (ms', es) <- checkTermsAreAll a wh ctx t' ms
        return (MSet t ms', [TSet t], es)


-- (t-map) ------------------------------------------------
checkTermWith a wh ctx Synth m@(MMap tk tv msk msv)
 = do
        when (not $ length msk == length msv)
         $ throw $ ErrorTermMalformed a wh m

        tk' <- checkTypeIs a wh ctx TData tk
        tv' <- checkTypeIs a wh ctx TData tv

        (msk', esKeys) <- checkTermsAreAll a wh ctx tk' msk
        (msv', esVals) <- checkTermsAreAll a wh ctx tv' msv
        return  ( MMap tk tv msk' msv'
                , [TMap tk tv]
                , esKeys ++ esVals)

checkTermWith a wh ctx (Check tsExpected) m
 = checkTermIs a wh ctx tsExpected m

checkTermWith _ _ _ mode mm
 =  error $ unlines
        [ "checkTerm: no match"
        , Text.ppShow mode
        , Text.ppShow mm
        , P.renderIndent $ P.ppr () mm ]

