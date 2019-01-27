
module Salt.Core.Check.Term.Proc where
import Salt.Core.Check.Term.Stmt
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import Salt.Core.Codec.Text             ()

import Text.Show.Pretty


-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
checkTermProc :: CheckTermProc a

-- (t-proc-seq) -------------------------------------------
checkTermProc a wh ctx tsReturn (MProcSeq mStmt mRest)
 = do
        (mStmt', esStmt)
         <- checkTermStmt a wh ctx tsReturn mStmt

        (mRest', esRest)
         <- checkTermProc a wh ctx tsReturn mRest

        return  ( MProcSeq mStmt' mRest'
                , esStmt ++ esRest)


-- (t-proc-let) --------------------------------------------
checkTermProc a wh ctx tsReturn (MProcLet mps mBind mRest)
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

        -- Check the rest of the.
        (mRest', esResult)
         <- checkTermProc a wh ctx' tsReturn mRest

        return  ( MLet mps' mBind' mRest'
                , esBind ++ esResult)


-- (t-proc-cel) -------------------------------------------
checkTermProc a wh ctx tsReturn (MProcCel nCel tCel mBind mRest)
 = do
        tCel' <- checkTypeHas UKind a wh ctx TData tCel

        (mBind', _tBind, esBind)
         <- checkTerm a wh (asExp ctx) (Check [tCel]) mBind

        let ctx' = contextBindTerm nCel (TCel tCel') ctx
        (mRest', esRest)
         <- checkTermProc a wh ctx' tsReturn mRest

        return  ( MProcCel nCel tCel' mBind' mRest'
                , esBind ++ esRest)


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


-----------------------------------------------------------
-- We don't know how to check this sort of procedure
checkTermProc _a _wh _ctx _tsResult mm
 = error $ ppShow ("procedure" :: String, mm)
