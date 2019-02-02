
module Salt.Core.Check.Term.Stmt where
-- import Salt.Core.Check.Term.Case
-- import Salt.Core.Check.Term.Base
-- import Salt.Core.Check.Type.Base
-- import Salt.Core.Codec.Text             ()
-- import Text.Show.Pretty


-- checkTermStmt :: CheckTermStmt a
{-
-- (t-stmt-proc) ------------------------------------------
checkTermStmt a wh ctx _tsReturn (MStmtProc tsResult mBody)
 = do   tsResult'
         <- checkTypesAreAll UType a wh ctx TData tsResult

        (mProc', esProc)
         <- contextCheckProc ctx a wh ctx tsResult' mBody

        return  ( MStmtProc tsResult mProc'
                , esProc)


-- (t-stmt-nest) ------------------------------------------
checkTermStmt a wh ctx tsReturn (MStmtNest mProc)
 = do
        (mProc', esProc)
         <- contextCheckProc ctx a wh ctx tsReturn mProc

        return  ( MStmtNest mProc'
                , esProc)


-- (t-stmt-if) --------------------------------------------
checkTermStmt a wh ctx tsReturn (MStmtIf msCond msThen)
 | length msCond == length msThen
 = do   (msCond', esCond)
         <- checkTermsAreAll a wh (asExp ctx) TBool msCond

        (msThen', essThen)
         <- fmap unzip
         $  mapM (checkTermStmt a wh ctx tsReturn)  msThen

        return  ( MStmtIf msCond' msThen'
                , esCond ++ concat essThen)


-- (t-stmt-case) ------------------------------------------
checkTermStmt a wh ctx tsReturn mCase@(MStmtCase mScrut msAlt)
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
        (msAlt', esResult)
         <- checkCaseStmtAlts a wh ctx mCase tScrut tsReturn nmgsScrut msAlt

        return  ( MStmtCase mScrut' msAlt'
                , esScrut ++ esResult)


-- (t-stmt-loop) ------------------------------------------
checkTermStmt a wh ctx tsReturn (MStmtLoop mBody)
 = do   (mBody', esBody)
         <- checkTermStmt a wh ctx tsReturn mBody

        return  ( MStmtLoop mBody'
                , esBody)


-- (t-stmt-update) ----------------------------------------
checkTermStmt a wh ctx _tsReturn (MStmtUpdate nCel mNew)
 = do   let uCel = BoundWith nCel 0
        tCel    <- contextResolveTermBound ctx uCel
                >>= \case
                        Nothing -> throw $ ErrorUnknownBound UTerm a wh uCel
                        Just t  -> return t

        tCel'   <- simplType a ctx tCel
        tVal    <- case tCel' of
                        TCel t  -> return t
                        _       -> throw $ ErrorProcUpdateNotCel a wh tCel'

        (mNew', _tsNew, esNew)
         <- checkTerm a wh (asExp ctx) (Check [tVal]) mNew

        return  ( MStmtUpdate nCel mNew'
                , esNew)


-- (t-stmt-break) -----------------------------------------
-- TODO: check we are syntactically within a loop.
checkTermStmt _a _wh _ctx _tsReturn m@MStmtBreak
 = do   return  (m, [])


-- (t-stmt-continue) -----------------------------------------
-- TODO: check we are syntactically within a loop.
checkTermStmt _a _wh _ctx _tsReturn m@MStmtContinue
 = do   return  (m, [])


-- (t-stmt-return) ----------------------------------------
checkTermStmt a wh ctx tsReturn (MStmtReturn mBody)
 = do   (mBody', _, esReturn)
         <- checkTerm a wh (asExp ctx) (Check tsReturn) mBody

        return  ( MStmtReturn mBody'
                , esReturn)


-----------------------------------------------------------
-- TODO: real error message.
checkTermStmt _ _ _ _ mm
 = error $ "checkTermStmt: "  ++ ppShow mm
-}
