
module Salt.Core.Check.Term.Alt where
import Salt.Core.Check.Term.Base
import Salt.Core.Codec.Text             ()
import qualified Salt.Data.List         as List


-- Check some case alternatives.
--  This same code is used to check alternatives in term case expressions,
--  as well as in procedure case statements. In the latter case we set the
--  incoming mode to (Return ts), where ts is the return type vector for
--  the statement form.
--
checkAlts
        :: Annot a
        => a -> [Where a] -> Context a
        -> Term a               -- ^ Entire case term, for error reporting.
        -> Type a               -- ^ Type of scrutinee, for error reporting.
        -> [(Name, TypeArgs a)] -- ^ Names and types of scrutinee fields.
        -> [Term a]             -- ^ Alternatives.
        -> Mode a               -- ^ Checker mode for the alternative bodies.
        -> IO ([Term a], [Type a], [Effect a])

checkAlts a wh ctx mCase tScrut nmgsScrut alts mode
 = go alts [] Nothing []
 where
    go  (MVarAlt nPat mpsPat mBody : msAltsRest)
        msAltsChecked mtsResult esAlt
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

        -- Check we don't have duplicate binders.
        let nsPat    = [n | BindName n <- map fst btsPat ]
        let nsPatDup = List.duplicates nsPat
        when (not $ null nsPatDup)
         $ throw $ ErrorCaseAltPatBindConflict aPatBind wh nPat nsPatDup

        -- Check that the pattern field types match the fields of the scrutinee.
        (checkTypeEquivs ctx a [] tsPat a [] tsField
         >>= \case
              Nothing -> return ()
              Just ((_a1, t1), (_a2, t2))
               -> throw $ ErrorCaseAltPatMismatch aPatBind wh nPat t1 t2)

        -- Check the result in the context extended by the fields
        -- we matched with the pattern. Also ensure this alternative
        -- has the same result type as any others we have checked before.
        let ctx'  = contextBindTermParams (MPTerms btsPat) ctx
        let mode'
                -- When checking the statement form the alts have the same mode
                -- as the overall expression.
                | Return{}      <- mode         = mode

                -- When checking the term form we check successive alts against
                -- the type of the first one.
                | Just ts       <- mtsResult    = Check ts
                | otherwise                     = Synth

        let aBody = fromMaybe a $ takeAnnotOfTerm mBody
        (mBody', tsResult, esResult)
         <- checkTerm aBody wh ctx' mode' mBody

        go  msAltsRest
            (MVarAlt nPat mpsPat mBody' : msAltsChecked)
            (Just tsResult)
            (esResult ++ esAlt)

    go [] msAltsChecked (Just tsResult) esAlt
     =  return  ( reverse msAltsChecked
                , tsResult
                , reverse esAlt)

    -- There are either no alternatives or one of them is not a MVarAlt term.
    go _ _ _ _
     = throw $ ErrorTermMalformed a wh mCase
