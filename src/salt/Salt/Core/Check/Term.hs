
module Salt.Core.Check.Term where
-- import Salt.Core.Check.Term.Proc
import Salt.Core.Check.Term.App
import Salt.Core.Check.Term.Case
import Salt.Core.Check.Term.Bind
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Value
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import Salt.Core.Codec.Text             ()
import qualified Salt.Core.Prim.Ops     as Prim
import qualified Salt.Core.Prim.Ctor    as Prim
import qualified Salt.Data.List         as List
import qualified Data.Map.Strict        as Map
-- import qualified Data.Set               as Set

-- import qualified Text.Show.Pretty       as Debug


------------------------------------------------------------------------------------------ Synth --
-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
synthTermWith :: SynthTerm a

-- (t-synth-ann) ------------------------------------------
synthTermWith _a wh ctx (MAnn a' m)
 = do   (m', t, eff)
         <- synthTerm a' wh ctx m
        return (MAnn a' m', t, eff)


-- (t-synth-mmm) ------------------------------------------
synthTermWith a wh ctx (MTerms msArg)
 = do   (msArg', tsArg, esArg)
         <- synthTerms a wh ctx msArg
        return  (MTerms msArg', tsArg, esArg)


-- (t-synth-the) ------------------------------------------
-- TODO: also add check form.
synthTermWith a wh ctx (MThe ts m)
 = do   -- TODO: check well kindedness of type annots.
        (m', es) <- checkTerm a wh ctx ts m
        return  (MThe ts m', ts, es)


-- (t-synth-box) ------------------------------------------
synthTermWith a wh ctx (MBox m)
 = do   (m', ts, es) <- synthTerm a wh ctx m
        tEff <- simplType a ctx (TSum es)
        return  (MBox m', [TSusp ts tEff], [])


-- (t-synth-run) ------------------------------------------
synthTermWith a wh ctx (MRun mBody)
 = do   -- Check the body.
        (mBody', tSusp, es)
         <- synthTerm1 a wh ctx mBody

        -- The body must produce a suspension.
        -- When we run it it causes the effects in its annotations.
        tSusp_red  <- simplType a ctx tSusp
        let aBody   = fromMaybe a $ takeAnnotOfTerm mBody
        case tSusp_red of
         TSusp tsResult' e'
            -> return (MRun mBody', tsResult', es ++ [e'])
         _  -> throw $ ErrorRunSuspensionIsNot aBody wh [tSusp_red]


-- (t-synth-val) ------------------------------------------
synthTermWith a wh ctx m@(MRef (MRVal v))
 = do   t <- synthValue a wh ctx v
        return (m, [t], [])


-- (t-synth-prm) ------------------------------------------
synthTermWith a wh ctx m@(MRef (MRPrm nPrim))
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
 = throw $ ErrorUnknownPrim UTerm a wh nPrim


-- (t-synth-con) ------------------------------------------
synthTermWith a wh ctx m@(MRef (MRCon nCon))
 = contextResolveDataCtor nCon ctx
 >>= \case
        Nothing
         -> throw $ ErrorUnknownCtor UTerm a wh nCon

        Just tCtor
         -> do  let tCtor' = mapAnnot (const a) tCtor
                return (m, [tCtor'], [])


-- (t-synth-var) ------------------------------------------
synthTermWith a wh ctx m@(MVar u)
 = contextResolveTermBound ctx u
 >>= \case
        -- Cells referenced in statements are implicitly read.
        --   The type 'Cell T' has kind #State, not #Data,
        --   so we cannot produce the Cell type itself.
        Just (TCell t)
         -> return (m, [t], [])

        Just t  -> return (m, [t], [])
        Nothing -> throw $ ErrorUnknownBound UTerm a wh u


-- (t-synth-abt) ------------------------------------------
synthTermWith a wh ctx (MAbs mps m)
 | Just bks <- takeMPTypes mps
 = do
        -- Check the parameters and bind into the context.
        mps' <- checkTermParams a wh ctx mps
        let ctx' = contextBindTermParams mps ctx

        -- Check the body of the abstraction in the new context.
        -- It needs to produce a single value.
        let aBody  = fromMaybe a $ takeAnnotOfTerm m
        (m', ts, es) <- synthTerm a wh ctx' m
        tBody
         <- case ts of
                []  -> throw $ ErrorAbsEmpty UType aBody wh
                [t] -> return t
                _   -> throw $ ErrorWrongArityUp UTerm a wh ts [TData]

        -- The body must be pure.
        eBody_red  <- simplType aBody ctx' (TSum es)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorAbsImpure UType aBody wh eBody_red

        return  (MAbs mps' m', [TForall (TPTypes bks) tBody], [])


-- (t-synth-abm) ------------------------------------------
synthTermWith a wh ctx (MAbs mps m)
 | Just bts <- takeMPTerms mps
 = do
        -- Check the parameters and bind them into the context.
        mps' <- checkTermParams a wh ctx mps
        let ctx' = contextBindTermParams mps ctx

        -- Check the body of the abstraction in the new context.
        (m', ts, es) <- synthTerm a wh ctx' m

        -- The body must be pure.
        let aBody  = fromMaybe a $ takeAnnotOfTerm m
        eBody_red  <- simplType a ctx' (TSum es)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorAbsImpure UTerm aBody wh eBody_red

        return  (MAbs mps' m', [TFun (map snd bts) ts], [])


-- (t-synth-aps) ------------------------------------------
-- This handles (t-apt), (t-apm) and (t-apv) from the docs.
synthTermWith a wh ctx (MAps mFun mgss)
 = checkTermApp a wh ctx mFun mgss


-- (t-synth-let) ------------------------------------------
-- TODO: also add check form.
synthTermWith a wh ctx (MLet mps mBind mBody)
 | (aParam, mps_) <- unwrapTermParams a mps
 , Just _bts      <- takeMPTerms mps_
 = do
        -- Check kinds of binder annotations.
        mps' <- checkTermParams a wh ctx mps

        -- Check the bound expression.
        (mBind', tsBind, esBind)
         <- synthTerm a wh ctx mBind

        -- Check we have the same number of binders
        -- as values produced by the binding.
        let Just (bs, tsParam) = fmap unzip $ takeMPTerms mps'
        when (not $ length tsParam == length tsBind)
         $ throw $ ErrorLetWrongArity aParam wh tsBind bs

        -- Check binding types against any annotations for them,
        -- then add them to the context.
        let aBind = fromMaybe a $ takeAnnotOfTerm mBind
        let checkLetAnnot tAnnot tBind
             | THole    <- tAnnot
             = return tBind

             | otherwise
             = checkTypeEquiv ctx a [] tBind a [] tAnnot
             >>= \case
                Nothing -> return tBind
                Just ((_a1, tErr1), (_a2, tErr2))
                  -> throw $ ErrorMismatch UType aBind wh tErr1 tErr2

        tsBind'   <- zipWithM checkLetAnnot tsParam tsBind
        let bts'' = zip bs tsBind'
        let ctx'  = contextBindTermParams (MPTerms bts'') ctx

        -- Check the body term.
        (mBody', tsResult, esResult)
         <- synthTerm a wh ctx' mBody

        return  ( MLet mps' mBind' mBody'
                , tsResult, esBind ++ esResult)


-- (t-synth-rec) ------------------------------------------
-- TODO: also add check form.
synthTermWith a wh ctx (MRec bms mBody)
 = do
        -- Check the type annotations on each of the binders.
        let tsBind  = map makeTypeOfTermBind bms
        tsBind' <- checkTypesAreAll UKind a wh ctx TData tsBind

        -- Check for duplicate recursive binders.
        let nsBind  = mapMaybe takeNameOfTermBind bms
        let nsDup   = List.duplicates nsBind
        when (not $ null nsDup)
         $ throw $ ErrorRecConflict a wh nsDup

        -- Check the bindings, with the types of each in scope.
        let btsBind = [ (bindOfTermBind bm, t) | bm <- bms | t <- tsBind']
        let ntsBind = [ (n, t) | (BindName n, t) <- btsBind ]
        let ctx'    = contextBindTerms ntsBind ctx
        bms'    <- mapM (checkTermBind a wh ctx') bms

        (mBody', tsResult, esResult)
         <- synthTerm a wh ctx' mBody

        return  ( MRec bms' mBody'
                , tsResult, esResult)


-- (t-synth-rcd) ------------------------------------------
synthTermWith a wh ctx (MRecord ns ms)
 = do
        -- Check for duplicate fields.
        let nsDup = List.duplicates ns
        when (not $ null nsDup)
         $ throw $ ErrorRecordDuplicateFields a wh nsDup

        -- Check each of the field terms.
        (ms', tss', ess')
         <- fmap unzip3 $ mapM (synthTerm a wh ctx) ms

        return  ( MRecord ns ms'
                , [TRecord ns (map TGTypes tss')]
                , concat ess')


-- (t-synth-prj) ------------------------------------------
synthTermWith a wh ctx (MProject nLabel mRecord)
 = do
        -- Check the body expression.
        let aRecord = fromMaybe a $ takeAnnotOfTerm mRecord
        (mRecord', tRecord, esRecord)
         <- synthTerm1 aRecord wh ctx mRecord

        -- The body needs to have record type with the field that we were expecting.
        (ns, tgss, tRecord')
         <- simplType a ctx tRecord
         >>= \case
                t@(TRecord ns tgss) -> return (ns, tgss, t)
                tThing  -> throw $ ErrorRecordProjectIsNot aRecord wh tThing nLabel

        -- Lookup the types of the field.
        tsField
         <- case lookup nLabel $ zip ns tgss of
                Just tgs -> return $ takeTGTypes tgs
                Nothing  -> throw $ ErrorRecordProjectNoField aRecord wh tRecord' nLabel

        return  ( MProject nLabel mRecord'
                , tsField, esRecord)


-- (t-synth-vnt) ------------------------------------------
synthTermWith a wh ctx (MVariant nLabel mValues tVariant)
 = do
        -- Check annotation is well kinded.
        checkType a wh ctx tVariant

        -- The annotation tells us what type to expect for the body.
        let aAnnot = fromMaybe a $ takeAnnotOfType tVariant
        (ns, tgss, tVariant')
         <- simplType a ctx tVariant
         >>= \case
                t@(TVariant ns tgss) -> return (ns, tgss, t)
                tThing -> throw $ ErrorVariantAnnotIsNot aAnnot wh tThing

        -- Lookup the types of the alternative.
        tsExpected'
         <- case lookup nLabel $ zip ns tgss of
                Just tgs -> return $ takeTGTypes tgs
                _ -> throw $ ErrorVariantAnnotAltMissing aAnnot wh tVariant' nLabel

        -- Check the body against the type from the annotation.
        (mValues', esValues)
         <- checkTerm a wh ctx tsExpected' mValues

        return  ( MVariant nLabel mValues' tVariant
                , [tVariant], esValues)


-- (t-synth-cse) ------------------------------------------
-- TODO: also add check form.
synthTermWith a wh ctx mCase@(MVarCase mScrut msAlt msElse)
 | length msAlt  >= 1
 , length msElse <= 1
 = do
        -- Check the scrutinee.
        (mScrut', tScrut, esScrut)
         <- synthTerm1 a wh ctx mScrut

        -- The scrutinee needs to be a variant.
        let aScrut = fromMaybe a $ takeAnnotOfTerm mScrut
        (nsScrut, mgsScrut)
         <- simplType aScrut ctx tScrut
         >>= \case
                TVariant ns mgs -> return (ns, mgs)
                _ -> throw $ ErrorCaseScrutNotVariant aScrut wh tScrut

        -- Check all alternatives in turn,
        --  collecting up all the effects,
        --  and ensuring all the alt result types match.
        let nmgsScrut = zip nsScrut mgsScrut
        (msAlt', tsResult, esResult)
         <- checkCaseTermAlts a wh ctx mCase tScrut nmgsScrut msAlt

        -- Check the default 'else' branch if we have one.
        (mmElse', esElse)
         <- case listToMaybe msElse of
                Nothing
                 -> return (Nothing, [])
                Just mElse
                 -> do  (mElse', esElse)
                         <- checkTerm a wh ctx tsResult mElse
                        return (Just mElse', esElse)

        return  ( MVarCase mScrut' msAlt' (maybeToList mmElse')
                , tsResult
                , esScrut ++ esResult ++ esElse)


-- (t-synth-ifs) ------------------------------------------
synthTermWith a wh ctx (MIf msCond msThen mElse)
 | length msCond == length msThen
 = do
        (msCond', esCond)
         <- checkTermsAreAll a wh ctx TBool msCond

        (mElse', tsElse, esElse)
         <- synthTerm a wh ctx mElse

        (msThen', essThen)
         <- fmap unzip
         $  mapM (checkTerm a wh ctx tsElse) msThen

        return  ( MIf msCond' msThen' mElse'
                , tsElse
                , esCond ++ concat essThen ++ esElse)


-- (t-synth-lst) ------------------------------------------
synthTermWith a wh ctx (MList t ms)
 = do   t' <- checkTypeHas UKind a wh ctx TData t
        (ms', es) <- checkTermsAreAll a wh ctx t' ms
        return  (MList t' ms', [TList t], es)


-- (t-synth-set) ------------------------------------------
synthTermWith a wh ctx (MSet t ms)
 = do   t' <- checkTypeHas UKind a wh ctx TData t
        (ms', es) <- checkTermsAreAll a wh ctx t' ms
        return (MSet t ms', [TSet t], es)


-- (t-synth-map) ------------------------------------------
synthTermWith a wh ctx m@(MMap tk tv msk msv)
 = do
        when (not $ length msk == length msv)
         $ throw $ ErrorTermMalformed a wh m

        tk' <- checkTypeHas UKind a wh ctx TData tk
        tv' <- checkTypeHas UKind a wh ctx TData tv

        (msk', esKeys) <- checkTermsAreAll a wh ctx tk' msk
        (msv', esVals) <- checkTermsAreAll a wh ctx tv' msv
        return  ( MMap tk tv msk' msv'
                , [TMap tk tv]
                , esKeys ++ esVals)


-- (t-synth-private) --------------------------------------
-- TODO: add check form.
synthTermWith a wh ctx (MPrivate bksR btsW mBody)
 = do
        -- TODO keep unpacking and repacking using MPTypes/MPTerms
        --      instead we should just change MPrivate type
        (MPTypes bksR') <- checkTermParams a wh ctx (MPTypes bksR)

        -- introduce region to context
        let ctx'  = contextBindTermParams (MPTypes bksR') ctx

        -- Check that all witness bindings have type TProp
        let (bs, ts) = unzip btsW
        ts' <- checkTypesAre UType a wh ctx' (replicate (length ts) TProp) ts
        let btsW' = zip bs ts'

        -- introduce capability witnesses to context
        let ctx'' = contextBindTermParams (MPTerms btsW') ctx'

        -- check the body term in new ctx
        (mBody', tsResult, esResult)
         <- synthTerm a wh ctx'' mBody

        return  ( MPrivate bksR' btsW' mBody'
                , tsResult, esResult)


-- (t-synth-extend) ---------------------------------------
-- TODO: add check form.
synthTermWith a wh ctx (MExtend r1 bksR btsW mBody)
 = do
        -- TODO keep unpacking and repacking using MPTypes/MPTerms
        --      instead we should just change MExtend type

        -- check from region
        checkType a wh ctx r1

        -- check region bindings
        (MPTypes bksR') <- checkTermParams a wh ctx (MPTypes bksR)

        -- introduce region to context
        let ctx'  = contextBindTermParams (MPTypes bksR') ctx

        -- Check that all witness bindings have type TProp
        let (bs, ts) = unzip btsW
        ts' <- checkTypesAre UType a wh ctx' (replicate (length ts) TProp) ts
        let btsW' = zip bs ts'

        -- introduce capability witnesses to context
        let ctx'' = contextBindTermParams (MPTerms btsW') ctx'

        -- check the body term in new ctx
        (mBody', tsResult, esResult)
         <- synthTerm a wh ctx'' mBody

        return (MExtend r1 bksR' btsW' mBody', tsResult, esResult)


-- (t-synth-launch) ---------------------------------------
synthTermWith a wh ctx (MLaunch tsResult mBody)
 = do
        tsResult'
         <- checkTypesAreAll UType a wh ctx TData tsResult

        (mBody', _tsResult, esResult)
         <- synthTerm a wh
                (goInside (InsideLaunch tsResult') ctx)
                mBody

        return  ( MLaunch tsResult' mBody'
                , tsResult', esResult)


-- (t-synth-return) ---------------------------------------
-- TODO: add check form.
synthTermWith a wh ctx (MReturn mBody)
 = do
        -- Get the expected type from the enclosing launch construct,
        -- or error if there isn't one. We can only return to a launch.
        tsResult
         <- case takeInnerLaunch (contextInside ctx) of
                Just tsResult -> return tsResult
                _  -> throw $ ErrorProcReturnNoLaunch a wh

        (mBody', esReturn)
         <- checkTermHas a wh ctx tsResult mBody

        return  ( MReturn mBody'
                , [TReturn tsResult], esReturn)


-- (t-synth-cell) -----------------------------------------
-- TODO: add check form.
synthTermWith a wh ctx (MCell nCell tCell mBind mRest)
 = do
        tCell' <- checkTypeHas UKind a wh ctx TData tCell

        (mBind', esBind)
         <- checkTerm a wh ctx [tCell] mBind

        let ctx' = contextBindTerm nCell (TCell tCell') ctx
        (mRest', tsResult, esRest)
         <- synthTerm a wh ctx' mRest

        return  ( MCell nCell tCell' mBind' mRest'
                , tsResult, esBind ++ esRest)


-- (t-synth-update) ---------------------------------------
synthTermWith a wh ctx (MUpdate nCell mNew mRest)
 = do
        let uCell = BoundWith nCell 0
        tCell   <- contextResolveTermBound ctx uCell
                >>= \case
                        Nothing -> throw $ ErrorUnknownBound UTerm a wh uCell
                        Just t  -> return t

        tCell'  <- simplType a ctx tCell
        tVal    <- case tCell' of
                        TCell t -> return t
                        _       -> throw $ ErrorProcUpdateNotCell a wh tCell'

        (mNew', esNew)
         <- checkTerm a wh ctx [tVal] mNew

        (mRest', tsResult, esRest)
         <- synthTerm a wh ctx mRest

        return  ( MUpdate nCell mNew' mRest'
                , tsResult, esNew ++ esRest)


-- (t-synth-whens) ----------------------------------------
synthTermWith a wh ctx (MWhens msCond msThen mRest)
 | length msCond == length msThen
 = do
        (msCond', esCond)
         <- checkTermsAreAll a wh ctx TBool msCond

        (msThen', essThen)
         <- fmap unzip
         $  mapM (checkTerm a wh ctx [])  msThen

        (mRest', tsResult, esRest)
         <- synthTerm a wh ctx mRest

        return  ( MWhens msCond' msThen' mRest'
                , tsResult, esCond ++ concat essThen ++ esRest)


-- (t-synth-match) ----------------------------------------
synthTermWith a wh ctx mCase@(MMatch mScrut msAlt mRest)
 = do
        -- Check the scrutinee.
        (mScrut', tScrut, esScrut)
         <- synthTerm1 a wh ctx mScrut

        -- The scrutinee needs to be a variant.
        let aScrut = fromMaybe a $ takeAnnotOfTerm mScrut
        (nsScrut, mgsScrut)
         <- simplType aScrut ctx tScrut
         >>= \case
                TVariant ns mgs -> return (ns, mgs)
                _ -> throw $ ErrorCaseScrutNotVariant aScrut wh tScrut

        -- Check all alternatives in turn,
        --  collecting up all the effects,
        --  and ensuring all the alt result types match.
        let nmgsScrut = zip nsScrut mgsScrut
        (msAlt', esAlt)
         <- checkCaseProcAlts a wh ctx mCase tScrut nmgsScrut msAlt

        -- Check the rest of the procedure.
        (mRest', tsResult, esRest)
         <- synthTerm a wh ctx mRest

        return  ( MMatch mScrut' msAlt' mRest'
                , tsResult, esScrut ++ esAlt ++ esRest)


-- (t-synth-loop) -----------------------------------------
synthTermWith a wh ctx (MLoop mBody mRest)
 = do
        -- Check the body of the loop.
        (mBody', esBody)
         <- checkTerm a wh
                (goInside InsideLoop ctx)
                [] mBody

        -- Check the rest of the procedure.
        (mRest', tsResult, esRest)
         <- synthTerm a wh ctx mRest

        return  ( MLoop mBody' mRest'
                , tsResult, esBody ++ esRest)


-- (t-synth-break) ----------------------------------------
synthTermWith a wh ctx MBreak
 = if areInsideLoop (contextInside ctx)
    then return (MBreak, [], [])
    else throw $ ErrorProcBreakNoLoop a wh


-- (t-synth-continue) -------------------------------------
synthTermWith a wh ctx MContinue
 = if areInsideLoop (contextInside ctx)
    then return (MContinue, [], [])
    else throw $ ErrorProcContinueNoLoop a wh


-- (t-synth-while) ----------------------------------------
synthTermWith a wh ctx (MWhile mPred mBody mRest)
 = do
        -- Check the predicate.
        (mPred', esPred)
         <- checkTerm a wh ctx [TBool] mPred

        -- Check the body of the loop.
        (mBody', esBody)
         <- checkTerm a wh
                (goInside InsideLoop ctx)
                [] mBody

        -- Check the rest of the procedure.
        (mRest', tsResult, esRest)
         <- synthTerm a wh ctx mRest

        return  ( MWhile mPred' mBody' mRest'
                , tsResult, esPred ++ esBody ++ esRest)


-- (t-synth-enter) ----------------------------------------
synthTermWith a wh ctx (MEnter mEnter bms mRest)
 = do
        -- Check the type annotations on each of the binders.
        let tsBind  = map makeTypeOfTermBind bms
        tsBind' <- checkTypesAreAll UKind a wh ctx TData tsBind

        -- Check for duplicate recursive binders.
        let nsBind  = mapMaybe takeNameOfTermBind bms
        let nsDup   = List.duplicates nsBind
        when (not $ null nsDup)
         $ throw $ ErrorRecConflict a wh nsDup

        -- Check the bindings, with the types of each in scope.
        let btsBind = [ (bindOfTermBind bm, t) | bm <- bms | t <- tsBind']
        let ntsBind = [ (n, t) | (BindName n, t) <- btsBind ]
        let ctx'    = contextBindTerms ntsBind ctx
        (bms', essBind)
         <- fmap unzip $ mapM (checkTermProcBind a wh ctx') bms

        -- Check the entry expression, with the binders in scope.
        (mEnter', esEnter)
         <- checkTermHas a wh ctx' [] mEnter

        -- Check the rest of the procedure.
        (mRest', tsResult, esRest)
         <- synthTerm a wh ctx mRest

        return  ( MEnter mEnter' bms' mRest'
                , tsResult, esEnter ++ concat essBind ++ esRest)


-- (t-leave) -----------------------------------------
synthTermWith _a _wh _ctx MLeave
 = do
        -- TODO: check we are in scope of an 'enter'
        return  ( MLeave
                , [], [])

-- fail --------------------------------------------
-- We don't know how to check this sort of term.
synthTermWith a wh _ctx mm
 = throw $ ErrorTermMalformed a wh mm


-- (t-check) ----------------------------------------------
-- Switch modes in bidirectional type checking.
--  We don't have an explicit check rule for this term,
--  so synthesise result types for it then compare the expected types
--  against the synthesised types.
checkTermWith a wh ctx tsExpected m
 = checkTermHas a wh ctx tsExpected m


------------------------------------------------------------------------------------------ Check --
--        mode' <- simplMode a ctx mode
--        case mode' of
--         -- If we have an expected type for all the fields then check the fields
--         -- separately. This gives better error messages as we don't need to report
--         -- types for fields that are irrelevant to the problem.
--         Check [TRecord ns' tgsExpected]
--          | Set.fromList ns == Set.fromList ns'
--          , Set.size (Set.fromList ns)  == length ns
--          , Set.size (Set.fromList ns') == length ns'
--          -> do
--                -- Check each of the field terms against the expected type for it.
--                let nts   = Map.fromList $ zip ns' tgsExpected
--                let nmtgs = [ let Just tg = Map.lookup n nts in (n, m, tg)
--                            | m  <- ms | n <- ns ]
--
--                -- When we do this we set the 'where' to the specific field we
--                -- are checking, which makes the error messages easier to read.
--                (ms', tss', ess')
--                  <- fmap unzip3 $ forM nmtgs $ \(n, m, tgs)
--                  -> do let ts  = takeTGTypes tgs
--                        let wh' = WhereRecordField a n (Just ts) : wh
--                        checkTerm a wh' ctx (Check ts) m
--
--                return  ( MRecord ns ms'
--                        , [TRecord ns $ map TGTypes tss']
--                        , concat ess')
--
--         -- The expected type we have doesn't cover all the fields of the record
--         -- being constructed. We call the default checker and let that fail.
--         Check tsExpected
--          -> checkTermHas a wh ctx tsExpected mm
--
--         -- Synthesise a type for the record.
--         Synth
--          -> do


----------------------------------------------------------------------------------- TermProcBind --
-- | Check a `TermProcBind`.
checkTermProcBind
        :: Annot a => a -> [Where a]
        -> Context a -> TermBind a -> IO (TermBind a, [Effect a])

checkTermProcBind a wh ctx (MBind b mpss tResult mBody)
 = do   -- Check the parameters.
        (ctx', mpss')
         <- checkTermParamss a wh ctx mpss

        -- There must be at least one vector of term parameters,
        -- as we do not support value recursion in the evaluator.
        when (not $ any isJust $ map takeMPTerms mpss)
         $ throw $ ErrorRecValueRecursion a wh b

        -- Check the result type annotation.
        tsResult'
         <- checkTypesAreAll UKind a wh ctx' TData tResult

        -- The body must have type as specified by the result annotation.
        (mBody', esBody)
         <- checkTerm a wh ctx' tsResult' mBody

        return  ( MBind b mpss' tsResult' mBody'
                , esBody)
