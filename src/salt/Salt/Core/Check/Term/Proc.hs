
module Salt.Core.Check.Term.Proc where
import Salt.Core.Check.Term.Case
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Bind
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import Salt.Core.Codec.Text             ()
import qualified Salt.Data.List         as List

import Text.Show.Pretty


-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
checkTermProc :: CheckTermProc a

-- (t-proc-yield) -----------------------------------------
checkTermProc a wh ctx mode _ctxProc (MProcYield mResult)
 = do
        let ctx' = ctx { contextFragment = FragProcYield }
        (mResult', tsResult, esResult)
         <- checkTerm a wh ctx' mode mResult

        return  ( MProcYield mResult'
                , tsResult, esResult)


-- (t-proc-call) ------------------------------------------
-- Checking of procedure calls is similar to checking term applications,
-- except that the form of the arguments is restricted. We use the exising
-- term application checker to check procedure calls.
checkTermProc a wh ctx mode _ctxProc (MProcCall mFun mgssArg)
 = do
        (MAps mFun' mgssArg', tsResult, esCall)
         <- checkTerm a wh (asExp ctx) mode (MAps mFun mgssArg)

        return  ( MProcCall mFun' mgssArg'
                , tsResult, esCall)


-- (t-proc-seq) --------------------------------------------
checkTermProc a wh ctx mode ctxProc (MProcSeq mps mBind mRest)
 | (aParam, mps_) <- unwrapTermParams a mps
 , Just _bts      <- takeMPTerms mps_
 = do
        -- Check kinds of binder annotations.
        mps' <- checkTermParams a wh ctx mps

        -- Check the bound procedure.
        (mBind', tsBind, esBind)
         <- checkTermProc a wh (asExp ctx) Synth ctxProc mBind

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

        -- Check the rest of the procedure.
        (mRest', tsResult, esResult)
         <- checkTermProc a wh ctx' mode ctxProc mRest

        return  ( MProcSeq mps' mBind' mRest'
                , tsResult, esBind ++ esResult)


-- (t-proc-launch) ----------------------------------------
checkTermProc a wh ctx _mode ctxProc (MProcLaunch tsResult mBody)
 = do
        tsResult'
         <- checkTypesAreAll UType a wh ctx TData tsResult

        (mBody', _tsResult, esResult)
         <- checkTermProc a wh ctx
                (Check tsResult')
                (CPLaunch tsResult' ctxProc)
                mBody

        return  ( MProcLaunch tsResult' mBody'
                , tsResult, esResult)


-- (t-proc-return) ----------------------------------------
checkTermProc a wh ctx _mode ctxProc (MProcReturn mBody)
 = do
        -- Get the expected type from the enclosing launch constructor,
        --  or error if there isn't one.
        tsResult
         <- case takeInnerLaunch ctxProc of
                Just tsResult -> return tsResult
                _  -> throw $ ErrorProcReturnNoLaunch a wh

        (mBody', _tsResult, esReturn)
         <- checkTermHas a wh (asExp ctx) tsResult mBody

        return  ( MProcReturn mBody'
                , []
                , esReturn)


-- (t-proc-cell) ------------------------------------------
checkTermProc a wh ctx mode ctxProc (MProcCell nCell tCell mBind mRest)
 = do
        tCell' <- checkTypeHas UKind a wh ctx TData tCell

        (mBind', _tBind, esBind)
         <- checkTerm a wh (asExp ctx) (Check [tCell]) mBind

        let ctx' = contextBindTerm nCell (TCell tCell') ctx
        (mRest', tsResult, esRest)
         <- checkTermProc a wh ctx' mode ctxProc mRest

        return  ( MProcCell nCell tCell' mBind' mRest'
                , tsResult, esBind ++ esRest)


-- (t-proc-update) ----------------------------------------
checkTermProc a wh ctx mode ctxProc (MProcUpdate nCell mNew mRest)
 = do   let uCell = BoundWith nCell 0
        tCell   <- contextResolveTermBound ctx uCell
                >>= \case
                        Nothing -> throw $ ErrorUnknownBound UTerm a wh uCell
                        Just t  -> return t

        tCell'  <- simplType a ctx tCell
        tVal    <- case tCell' of
                        TCell t -> return t
                        _       -> throw $ ErrorProcUpdateNotCell a wh tCell'

        (mNew', _tsNew, esNew)
         <- checkTerm a wh (asExp ctx) (Check [tVal]) mNew

        (mRest', tsResult, esRest)
         <- checkTermProc a wh ctx mode ctxProc mRest

        return  ( MProcUpdate nCell mNew' mRest'
                , tsResult, esNew ++ esRest)


-- (t-proc-when) ------------------------------------------
checkTermProc a wh ctx mode ctxProc (MProcWhen msCond msThen mRest)
 | length msCond == length msThen
 = do   (msCond', esCond)
         <- checkTermsAreAll a wh (asExp ctx) TBool msCond

        (msThen', _tsThen, essThen)
         <- fmap unzip3
         $  mapM (checkTermProc a wh ctx (Check []) ctxProc)  msThen

        (mRest', tsResult, esRest)
         <- checkTermProc a wh ctx mode ctxProc mRest

        return  ( MProcWhen msCond' msThen' mRest'
                , tsResult, esCond ++ concat essThen ++ esRest)


-- (t-proc-match) -----------------------------------------
checkTermProc a wh ctx mode ctxProc mCase@(MProcMatch mScrut msAlt mRest)
 = do
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
        (msAlt', esAlt)
         <- checkCaseProcAlts a wh ctx mCase tScrut ctxProc nmgsScrut msAlt

        -- Check the rest of the procedure.
        (mRest', tsResult, esRest)
         <- checkTermProc a wh ctx mode ctxProc mRest

        return  ( MProcMatch mScrut' msAlt' mRest'
                , tsResult, esScrut ++ esAlt ++ esRest)


-- (t-proc-loop) ------------------------------------------
checkTermProc a wh ctx mode ctxProc (MProcLoop mBody mRest)
 = do
        -- Check the body of the loop.
        let ctxProc' = CPLoop ctxProc
        (mBody', _tsBody, esBody)
         <- checkTermProc a wh ctx (Check []) ctxProc' mBody

        -- Check the rest of the procedure.
        (mRest', tsResult, esRest)
         <- checkTermProc a wh ctx mode ctxProc mRest

        return  ( MProcLoop mBody' mRest'
                , tsResult, esBody ++ esRest)


-- (t-proc-enter) -----------------------------------------
checkTermProc a wh ctx mode ctxProc (MProcEnter mEnter bms mRest)
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
        bms'    <- mapM (checkTermProcBind a wh ctx' ctxProc) bms

        -- Check the entry expression, with the binders in scope.
        let ctxt    = ctx' { contextFragment = FragTerm }
        (mEnter', _tsEnter, esEnter)
         <- checkTermHas a wh ctxt [] mEnter

        -- Check the rest of the procedure.
        (mRest', tsResult, esRest)
         <- checkTermProc a wh ctx mode ctxProc mRest

        return  ( MProcEnter mEnter' bms' mRest'
                , tsResult, esEnter ++ esRest)


-- (t-proc-leave) -----------------------------------------
checkTermProc _a _wh _ctx _mode _ctxProc MProcLeave
 = do
        -- TODO: check we are in scope of an 'enter'
        return  ( MProcLeave
                , [], [])


-----------------------------------------------------------
-- We don't know how to check this sort of procedure
checkTermProc _a _wh _ctx _mode _ctxProc  mm
 = error $ ppShow ("checkTermProc" :: String, mm)



----------------------------------------------------------------------------------- TermProcBind --
-- | Check a `TermProcBind`.
checkTermProcBind
        :: Annot a => a -> [Where a]
        -> Context a -> ContextProc a -> TermBind a -> IO (TermBind a)

checkTermProcBind a wh ctx ctxProc (MBind b mpss tResult mBody)
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
        (mBody', _tsResult, esBody)
         <- checkTermProc a wh ctx' (Check tsResult') ctxProc mBody

        -- The body must be pure.
        let aBody  = fromMaybe a $ takeAnnotOfTerm mBody
        eBody_red  <- simplType aBody ctx' (TSum esBody)
        when (not $ isTPure eBody_red)
         $ throw $ ErrorAbsImpure UTerm aBody wh eBody_red

        return $ MBind b mpss' tsResult' mBody'
