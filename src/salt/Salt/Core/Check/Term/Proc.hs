
module Salt.Core.Check.Term.Proc where
-- import Salt.Core.Check.Term.Stmt
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import Salt.Core.Codec.Text             ()

import Text.Show.Pretty


-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
checkTermProc :: CheckTermProc a

-- (t-proc-yield) -----------------------------------------
checkTermProc a wh ctx mode _ctxProc (MProcYield mResult)
 = do
        (mResult', tsResult, esResult)
         <- checkTerm a wh (asExp ctx) mode mResult

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

        -- Check the bound expression.
        -- TODO: pass down expected type if we have it on the binder.
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




-----------------------------------------------------------
-- We don't know how to check this sort of procedure
checkTermProc _a _wh _ctx _mode _ctxProc  mm
 = error $ ppShow ("checkTermProc" :: String, mm)

{-
-- (t-proc-end-with) -------------------------------------
checkTermProc a wh ctx tsReturn (MProcEndWith mResult)
 = do
        (mResult', _tsResult, esResult)
         <- checkTerm a wh ctx (Check tsReturn) mResult

        return  ( MProcEndWith mResult'
                , esResult)


-- (t-proc-end) ------------------------------------------
checkTermProc _a _wh _ctx _tsReturn MProcEnd
 = do
        return  (MProcEnd, [])

-}
