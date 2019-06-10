
module Salt.Core.Check.Term.Case where
import Salt.Core.Check.Term.Base
import Salt.Core.Codec.Text             ()
import qualified Salt.Data.List         as List
import qualified Data.Set               as Set


---------------------------------------------------------------------------------------------------
-- Check some case term alternatives.
--  This is used when checking term-level case expressions.
checkCaseTermAlts
        :: Annot a
        => a -> [Where a] -> Context a
        -> Term a               -- ^ Entire case term, for error reporting.
        -> Type a               -- ^ Type of scrutinee, for error reporting.
        -> [(Name, TypeArgs a)] -- ^ Names and types of scrutinee fields.
        -> [Term a]             -- ^ Alternatives.
        -> IO ([Term a], [Type a], [Effect a])

checkCaseTermAlts a wh ctx mCase tScrut nmgsScrut msAlts
 = do   checkCaseAltsPatterns a wh ctx tScrut nmgsScrut msAlts True
        checkAlts msAlts [] Nothing []
 where
  checkAlts (MVarAlt nPat mpsPat mBody : msAltsRest)
            msAltsChecked mtsResult esAlt
   | (_aPatBind, mpsPat_) <- unwrapTermParams a mpsPat
   , Just btsPat          <- takeMPTerms mpsPat_
   = do
        -- Check the result in the context extended by the fields from the
        -- pattern. Also ensure this alternative has the same result type as
        -- any others we have checked before.
        let aBody = fromMaybe a $ takeAnnotOfTerm mBody
        let ctx'  = contextBindTermParams (MPTerms btsPat) ctx

        -- If one of the previous alternatives might produce a result vector
        -- then result vectors produced by later alternatives must also have
        -- the same type.
        (mBody', mtsResult', esResult)
         <- case mtsResult of
                Just ts
                 -> do  (m, _rr, es) <- checkTerm aBody wh ctx' ts mBody
                        return (m, mtsResult, es)
                Nothing
                 -> do  (m, rr, es)  <- synthTerm aBody wh ctx' mBody
                        case rr of
                         Just ts -> return (m, Just ts,   es)
                         _       -> return (m, mtsResult, es)

        checkAlts msAltsRest
            (MVarAlt nPat mpsPat mBody' : msAltsChecked)
            mtsResult'
            (esResult ++ esAlt)

  checkAlts [] msAltsChecked (Just tsResult) esAlt
   =    return  ( reverse msAltsChecked
                , tsResult
                , reverse esAlt)

  -- There are either no alternatives or one of them is not a MVarAlt term.
  -- TODO: handle case where no alt produces a result vector.
  checkAlts _ _ _ _
     = throw $ ErrorTermMalformed a wh mCase


---------------------------------------------------------------------------------------------------
-- Check some case stmt alternatives.
--   This is used when checking stmt-level case expressions,
--   which may return from the enclosing procedure with a value vector.
checkCaseProcAlts
        :: Annot a
        => a -> [Where a] -> Context a
        -> Term a               -- ^ Entire case term, for error reporting.
        -> Type a               -- ^ Type of scrutinee, for error reporting.
        -> [(Name, TypeArgs a)] -- ^ Names and types of scrutinee fields.
        -> [Term a]             -- ^ Alternatives.
        -> IO ([Term a], [Effect a])

checkCaseProcAlts a wh ctx mCase tScrut nmgsScrut msAlts
 = do   checkCaseAltsPatterns a wh ctx tScrut nmgsScrut msAlts False
        checkAlts msAlts [] []
 where
  checkAlts (MVarAlt nPat mpsPat mBody : msAltsRest)
            msAltsChecked esAltsRest
   | (_aPatBind, mpsPat_) <- unwrapTermParams a mpsPat
   , Just btsPat          <- takeMPTerms mpsPat_
   = do
        -- Check the result in the context extended by the fields from the pattern.
        -- Also ensure this alternative has the same result type as any others we
        -- have checked before.
        let aBody = fromMaybe a $ takeAnnotOfTerm mBody
        let ctx'  = contextBindTermParams (MPTerms btsPat) ctx

        (mBody', _rr, esBody)
         <- contextCheckTerm ctx' aBody wh ctx' [] mBody

        checkAlts msAltsRest
            (MVarAlt nPat mpsPat mBody' : msAltsChecked)
            (esBody ++ esAltsRest)

  checkAlts [] msAltsChecked esAltsRest
   =    return  ( reverse msAltsChecked
                , reverse esAltsRest)

  -- There are either no alternatives or one of them is not a MVarAlt term.
  checkAlts _ _ _
     = throw $ ErrorTermMalformed a wh mCase


---------------------------------------------------------------------------------------------------
-- | Check that the patterns in some alternatives are well formed.
--   We don't need the bodies of the alternatives for this,
--   so can use this function for both the term and statement case forms.
checkCaseAltsPatterns
        :: Annot a
        => a -> [Where a] -> Context a
        -> Type a                       -- ^ Type of scrutinee, for error messages.
        -> [(Name, TypeArgs a)]         -- ^ Name and args of scrutinee type.
        -> [Term a]                     -- ^ Alternatives to check.
        -> Bool                         -- ^ Whether alternatives must be exhaustive.
        -> IO ()

checkCaseAltsPatterns a wh ctx tScrut ntgsScrut msAlt bExhaustive
 = do   checkNotOverlapping

        when bExhaustive
         $ checkNotInexhaustive

        checkPatterns msAlt
 where

        nsScrut = map fst ntgsScrut
        nsAlt   = [n | MVarAlt n _ _ <- msAlt]

        -- Check that the alternaives are not overlapping.
        checkNotOverlapping
         = do let nsAltDup = List.duplicates nsAlt
              when (not $ null nsAltDup)
               $ throw $ ErrorCaseAltsOverlapping a wh nsAltDup

        -- Check that the alternatives are not inexhaustive.
        checkNotInexhaustive
         = do let nsNot = Set.difference (Set.fromList nsScrut) (Set.fromList nsAlt)
              when (not $ Set.null nsNot)
               $ throw $ ErrorCaseAltsInexhaustive a wh (Set.toList nsNot) tScrut

        checkPatterns (mAlt@(MVarAlt nPat mpsPat _mBody) : msRest)
         = do   checkCaseAltPattern a wh ctx tScrut mAlt  ntgsScrut nPat mpsPat
                checkPatterns msRest

        checkPatterns (m : _)   = throw $ ErrorTermMalformed a wh m
        checkPatterns []        = return ()


---------------------------------------------------------------------------------------------------
-- | Check the pattern in an alternative is well formed.
checkCaseAltPattern
        :: Annot a
        => a -> [Where a] -> Context a
        -> Type a                       -- ^ Type of scrutinee, for error messages.
        -> Term a                       -- ^ Term of complete alt, for error messages.
        -> [(Name, TypeArgs a)]         -- ^ Names and args of scrutinee type.
        -> Name                         -- ^ Name of alternative variant.
        -> TermParams a                 -- ^ Parameters of alternative.
        -> IO ()

checkCaseAltPattern a wh ctx tScrut mAlt nmgsScrut nPat mpsPat
 | (aPatBind, mpsPat_) <- unwrapTermParams a mpsPat
 , Just btsPat         <- takeMPTerms mpsPat_
 = do
        -- Lookup the field types from the type of the scrutinee.
        -- The type of the scrutinee must cover this alternative.
        tsField
         <- case lookup nPat nmgsScrut of
              Just tgs -> return $ takeTGTypes tgs
              Nothing  -> throw  $ ErrorCaseAltNotInVariant a wh nPat tScrut

        -- Check we have the same number of pattern binders as fields.
        let tsPat = map snd btsPat
        when (not $ length tsPat == length tsField)
         $ throw $ ErrorCaseAltPatWrongArity aPatBind wh nPat tsPat tsField

        -- Check we don't have duplicate binders in the pattern.
        let nsPat    = [n | BindName n <- map fst btsPat ]
        let nsPatDup = List.duplicates nsPat
        when (not $ null nsPatDup)
         $ throw $ ErrorCaseAltPatBindConflict aPatBind wh nPat nsPatDup

        -- Check that the pattern field types match the fields of the scrutinee.
        checkTypeEquivs ctx a [] tsPat a [] tsField
         >>= \case
              Nothing -> return ()
              Just ((_a1, t1), (_a2, t2))
               -> throw $ ErrorCaseAltPatMismatch aPatBind wh nPat t1 t2

 | otherwise    = throw $ ErrorTermMalformed a wh mAlt