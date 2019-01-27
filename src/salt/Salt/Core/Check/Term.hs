
-- TODO: split out the stmt checker into its own function,
--       the TermMode thing in the context isn't really working.
--
module Salt.Core.Check.Term where
import Salt.Core.Check.Term.Proc
import Salt.Core.Check.Term.App
import Salt.Core.Check.Term.Case
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Value
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import Salt.Core.Codec.Text             ()
import qualified Salt.Core.Prim.Ops     as Prim
import qualified Salt.Core.Prim.Ctor    as Prim
import qualified Salt.Data.List         as List
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set

import Text.Show.Pretty

-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
checkTermWith :: CheckTerm a

-- (t-ann) ------------------------------------------------
checkTermWith _a wh ctx mode (MAnn a' m)
 = do   (m', t, eff) <- checkTerm a' wh ctx mode m
        return (MAnn a' m', t, eff)


-- (t-mmm) ------------------------------------------------
checkTermWith a wh ctx mode (MTerms msArg)
 = do   (msArg', tsArg, esArg)
         <- checkTerms a wh ctx mode msArg
        return  (MTerms msArg', tsArg, esArg)


-- (t-the) ------------------------------------------------
checkTermWith a wh ctx Synth (MThe ts m)
 = do   -- TODO: check well kindedness of type annots.
        (m', _, es) <- checkTerm a wh ctx (Check ts) m
        return  (MThe ts m', ts, es)


-- (t-box) ------------------------------------------------
checkTermWith a wh ctx Synth (MBox m)
 = guardOnlyTermMode a wh ctx "Explicit box" TermModePlain
 $ do   (m', ts, es) <- checkTerm a wh ctx Synth m
        tEff <- simplType a ctx (TSum es)
        return  (MBox m', [TSusp ts tEff], [])


-- (t-run) ------------------------------------------------
checkTermWith a wh ctx Synth (MRun mBody)
 = guardOnlyTermMode a wh ctx "Explicit run" TermModePlain
 $ do
        -- Check the body.
        (mBody', tsSusp', es)
         <- checkTerm a wh ctx Synth mBody

        -- The body must produce a suspension.
        -- When we run it it causes the effects in its annotations.
        tsSusp_red' <- simplTypes a ctx tsSusp'
        let aBody   = fromMaybe a $ takeAnnotOfTerm mBody
        case tsSusp_red' of
         [TSusp tsResult' e']
            -> return (MRun mBody', tsResult', es ++ [e'])
         _  -> throw $ ErrorRunSuspensionIsNot aBody wh tsSusp_red'


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
 = throw $ ErrorUnknownPrim UTerm a wh nPrim


-- (t-con) ------------------------------------------------
checkTermWith a wh ctx Synth m@(MRef (MRCon nCon))
 =  contextResolveDataCtor nCon ctx
 >>= \case
        Nothing
         -> throw $ ErrorUnknownCtor UTerm a wh nCon

        Just tCtor
         -> do  let tCtor' = mapAnnot (const a) tCtor
                return (m, [tCtor'], [])


-- (t-var) ------------------------------------------------
checkTermWith a wh ctx Synth m@(MVar u)
 =   contextResolveTermBound ctx u
 >>= \case
         Just t  -> return (m, [t], [])
         Nothing -> throw $ ErrorUnknownBound UTerm a wh u


-- (t-abt) ------------------------------------------------
checkTermWith a wh ctx Synth (MAbs mps m)
 | Just bks <- takeMPTypes mps
 = guardOnlyTermMode a wh ctx "Type abstraction" TermModePlain
 $ do
        -- Check the parameters and bind into the context.
        mps' <- checkTermParams a wh ctx mps
        let ctx' = contextBindTermParams mps ctx

        -- Check the body of the abstraction in the new context.
        -- It needs to produce a single value.
        let aBody  = fromMaybe a $ takeAnnotOfTerm m
        (m', ts, es) <- checkTerm a wh ctx' Synth m
        tBody
         <- case ts of
                []      -> throw $ ErrorAbsEmpty UType aBody wh
                [t]     -> return t
                _       -> throw $ ErrorWrongArityUp UTerm a wh ts [TData]

        -- The body must be pure.
        eBody_red  <- simplType aBody ctx' (TSum es)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorAbsImpure UType aBody wh eBody_red

        return  (MAbs mps' m', [TForall (TPTypes bks) tBody], [])


-- (t-abm) ------------------------------------------------
checkTermWith a wh ctx Synth (MAbs mps m)
 | Just bts <- takeMPTerms mps
 = guardOnlyTermMode a wh ctx "Term abstraction" TermModePlain
 $ do
        -- Check the parameters and bind them into the context.
        mps' <- checkTermParams a wh ctx mps
        let ctx' = contextBindTermParams mps ctx

        -- Check the body of the abstraction in the new context.
        (m', ts, es) <- checkTerm a wh ctx' Synth m

        -- The body must be pure.
        let aBody  = fromMaybe a $ takeAnnotOfTerm m
        eBody_red  <- simplType a ctx' (TSum es)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorAbsImpure UTerm aBody wh eBody_red

        return  (MAbs mps' m', [TFun (map snd bts) ts], [])


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

                 | otherwise
                 -> let aFun = fromMaybe a (takeAnnotOfTerm mFun0)
                    in  throw $ ErrorUnknownPrim UTerm aFun wh nPrm

                Nothing
                 -> checkTerm1 a wh (asExp ctx) Synth mFun0

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
             = do (msArg', tsResult, es') <- checkTermAppTerms a' wh (asExp ctx) aFun tFun msArg
                  checkApp tsResult (es ++ es') mgss (MGAnn a' (MGTerms msArg') : mgssAcc)

            -- (t-apv) -----
            checkApp [tFun] es (mps : mgss) mgssAcc
             | Just (a', mArg) <- takeAnnMGTerm a mps
             = do (mArg',  tsResult, es') <- checkTermAppTerm  a wh (asExp ctx) aFun tFun mArg
                  checkApp tsResult (es ++ es') mgss (MGAnn a' (MGTerm mArg') : mgssAcc)

            checkApp tsResult es [] mgssAcc
             = return (MAps mFun1 (reverse mgssAcc), tsResult, es)

            -- If the current function is multi valued and we still have arguments
            --   then we had a type abstraction that returned multiple values
            --   but haven't detected that when it was constructed.
            checkApp tsResult _ _ _
             = throw $ ErrorWrongArityUp UTerm a wh tsResult [TData]

        checkApp [tFun1] esFun1 mgss0 []


-- (t-let) ------------------------------------------------
checkTermWith a wh ctx modeBody (MLet mps mBind mBody)
 | (aParam, mps_) <- unwrapTermParams a mps
 , Just _bts      <- takeMPTerms mps_
 = do
        -- Check kinds of binder annotations.
        mps' <- checkTermParams a wh ctx mps

        -- Check the bound expression.
        (mBind', tsBind, esBind)
         <- checkTerm a wh (asExp ctx) Synth mBind

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
         <- checkTerm a wh ctx' modeBody mBody

        return  ( MLet mps' mBind' mBody'
                , tsResult, esBind ++ esResult)


-- (t-rec) ------------------------------------------------
checkTermWith a wh ctx mode mm@(MRecord ns ms)
 = do
        mode'   <- simplMode a ctx mode
        case mode' of
         Return{} -> error "TODO: return a value"
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
                  <- fmap unzip3 $ forM nmtgs $ \(n, m, tgs)
                  -> do let ts  = takeTGTypes tgs
                        let wh' = WhereRecordField a n (Just ts) : wh
                        checkTerm a wh' (asExp ctx) (Check ts) m

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
                  <- fmap unzip3 $ mapM (checkTerm a wh (asExp ctx) Synth) ms

                 return  ( MRecord ns ms'
                         , [TRecord ns (map TGTypes tss')]
                         , concat ess')



-- (t-prj) ------------------------------------------------
checkTermWith a wh ctx Synth (MProject nLabel mRecord)
 = do
        -- Check the body expression.
        let aRecord = fromMaybe a $ takeAnnotOfTerm mRecord
        (mRecord', tRecord, esRecord)
         <- checkTerm1 aRecord wh (asExp ctx) Synth mRecord

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


-- (t-vnt) ------------------------------------------------
checkTermWith a wh ctx Synth (MVariant nLabel mValues tVariant)
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
        (mValues', _tsValues, esValues)
         <- checkTerm a wh (asExp ctx) (Check tsExpected') mValues

        return  ( MVariant nLabel mValues' tVariant
                , [tVariant], esValues)


-- (t-cse) ------------------------------------------------
checkTermWith a wh ctx Synth mCase@(MVarCase mScrut msAlt msElse)
 | length msAlt  >= 1
 , length msElse <= 1
 = guardAnyTermMode a wh ctx
        "case-expression"
        [TermModePlain, TermModeProcStmt, TermModeBlocBody]
 $ do
        -- Check the scrutinee.
        (mScrut', tScrut, esScrut)
         <- checkTerm1 a wh (asExp ctx) Synth mScrut

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
         <- checkCaseAlts a wh ctx mCase tScrut nmgsScrut msAlt

        -- Check the default 'else' branch if we have one.
        (mmElse', _tElse, esElse)
         <- case listToMaybe msElse of
                Nothing
                 -> return (Nothing, tsResult, [])
                Just mElse
                 -> do  (mElse', tsElse, esElse)
                         <- checkTerm a wh ctx (Check tsResult) mElse
                        return (Just mElse', tsElse, esElse)

        return  ( MVarCase mScrut' msAlt' (maybeToList mmElse')
                , tsResult
                , esScrut ++ esResult ++ esElse)


-- (t-ifs) ------------------------------------------------
checkTermWith a wh ctx Synth (MIf msCond msThen mElse)
 | length msCond == length msThen
 = guardAnyTermMode a wh ctx
        "if-expression"
        [TermModePlain, TermModeProcBody, TermModeBlocBody]
 $ do
        (msCond', esCond)
         <- checkTermsAreAll a wh (asExp ctx) TBool msCond

        (mElse',  tsElse, esElse)
         <- checkTerm a wh ctx Synth mElse

        (msThen', _tssThen, essThen)
         <- fmap unzip3
         $  mapM (checkTerm a wh ctx (Check tsElse)) msThen

        return  ( MIf msCond' msThen' mElse'
                , tsElse
                , esCond ++ concat essThen ++ esElse)


-- (t-proc) -----------------------------------------------
checkTermWith a wh ctx Synth (MProc tsReturn mBody)
 = guardAnyTermMode a wh ctx
        "procedure"
        [TermModePlain, TermModeProcBody]
 $ do
        (tsReturn', _)
         <- checkTypes a wh ctx tsReturn

        let ctx' = ctx { contextTermMode = TermModeProcBody }
        (mBody', esBody)
         <- checkTermProc a wh ctx' tsReturn' mBody

        return  ( MProc tsReturn' mBody'
                , tsReturn, esBody)

-- (t-bloc) -----------------------------------------------
checkTermWith a wh ctx Synth (MBloc mBody)
 = guardAnyTermMode a wh ctx
        "Bloc definition"
        [TermModePlain, TermModeProcBody, TermModeBlocBody]
 $ do
        let ctx' = ctx { contextTermMode = TermModeBlocBody }

        (mBody', tsBody, esBody)
         <- checkTerm a wh ctx' Synth mBody

        return  ( MBloc mBody'
                , tsBody, esBody)

-- (t-lst) ------------------------------------------------
checkTermWith a wh ctx Synth (MList t ms)
 = do   t' <- checkTypeHas UKind a wh ctx TData t
        (ms', es) <- checkTermsAreAll a wh (asExp ctx) t' ms
        return  (MList t' ms', [TList t], es)


-- (t-set) ------------------------------------------------
checkTermWith a wh ctx Synth (MSet t ms)
 = do   t' <- checkTypeHas UKind a wh ctx TData t
        (ms', es) <- checkTermsAreAll a wh (asExp ctx) t' ms
        return (MSet t ms', [TSet t], es)


-- (t-map) ------------------------------------------------
checkTermWith a wh ctx Synth m@(MMap tk tv msk msv)
 = do
        when (not $ length msk == length msv)
         $ throw $ ErrorTermMalformed a wh m

        tk' <- checkTypeHas UKind a wh ctx TData tk
        tv' <- checkTypeHas UKind a wh ctx TData tv

        (msk', esKeys) <- checkTermsAreAll a wh (asExp ctx) tk' msk
        (msv', esVals) <- checkTermsAreAll a wh (asExp ctx) tv' msv
        return  ( MMap tk tv msk' msv'
                , [TMap tk tv]
                , esKeys ++ esVals)

-- (t-check) ----------------------------------------------
-- Switch modes in bidirectional type checking.
--  We don't have an explicit check rule for this term,
--  so synthesise result types for it then compare the expected types
--  against the synthesised types.
checkTermWith a wh ctx (Check tsExpected) m
 = checkTermIs a wh ctx tsExpected m

-- We don't know how to check this sort of term.
checkTermWith _a _wh _ctx mode mm
 = error $ ppShow (mode, mm)
 -- throw $ ErrorTermMalformed a wh mm

