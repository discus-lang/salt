
module Salt.Core.Check.Term.Base
        ( module Salt.Core.Check.Context
        , module Salt.Core.Check.Equiv
        , module Salt.Core.Check.Error
        , module Salt.Core.Check.Where
        , module Salt.Core.Check.Reduce
        , module Salt.Core.Transform.MapAnnot
        , module Salt.Core.Transform.Snv
        , module Salt.Core.Transform.Ups
        , module Salt.Core.Exp
        , module Control.Monad
        , module Control.Exception
        , module Data.Maybe
        , synthTerm
        , synthTermProductive,   synthTermProductive1
        , synthTermsConjunctive, synthTermsDisjunctive
        , checkTerm
        , checkTerm1
        , checkTermsAreEach, checkTermsAreAll
        , checkTermHas)
where
import Salt.Core.Check.Context
import Salt.Core.Check.Equiv
import Salt.Core.Check.Error
import Salt.Core.Check.Where
import Salt.Core.Check.Reduce
import Salt.Core.Transform.MapAnnot
import Salt.Core.Transform.Snv
import Salt.Core.Transform.Ups
import Salt.Core.Exp
import Control.Monad
import Control.Exception
import Data.Maybe


------------------------------------------------------------------------------------------ Synth --
-- | Synthesize the result type for a single term.
synthTerm :: SynthTerm a (Maybe [Type a])
synthTerm a wh ctx term
 = contextSynthTerm ctx a wh ctx term


-- | Like `synthTerm`, but require the term to be productive.
synthTermProductive :: SynthTerm a [Type a]
synthTermProductive a wh ctx m
 = do   (m', rr', es') <- synthTerm a wh ctx m
        case rr' of
         Nothing  -> error "TODO: not productive. add better error message."
         Just ts' -> return (m', ts', es')


-- | Like 'synthTerm', but require the term to be productive,
--   and to produce a single value.
synthTermProductive1 :: SynthTerm a (Type a)
synthTermProductive1 a wh ctx m
 = do   (m', rr', es') <- synthTerm a wh ctx m
        case rr' of
         Nothing  -> error "TODO: not productive. add better error message."
         Just [t] -> return (m', t, es')
         Just ts' -> throw $ ErrorWrongArityUp UTerm a wh ts' [TData]


-- | Synthesize types for each of the terms,
--   and combine them conjunctively into the same result vector.
--
--   If any of the terms is non-productive then treat the whole result
--   as non-productive, otherwise concatenate the results from each
--   of the terms into the overall result.
synthTermsConjunctive :: SynthTerms a (Maybe [Type a])
synthTermsConjunctive a wh ctx ms
 = do   (ms', rss', ess')
         <- fmap unzip3 $ mapM (synthTerm a wh ctx) ms
        let rs' = fmap concat $ sequence rss'
        return (ms', rs', concat ess')


-- | Synthesize types of each of the terms,
--   and combine them disjunctively into the same result vector.
synthTermsDisjunctive :: SynthTerms a (Maybe [Type a])
synthTermsDisjunctive a wh ctx ms
 = do   (ms', rss', ess')
         <- fmap unzip3 $ mapM (synthTerm a wh ctx) ms

        rs' <- mergeResults a wh ctx rss'
        return (ms', rs', concat ess')


------------------------------------------------------------------------------------------ Check --
-- | Check a term against the expected types of its result values.
checkTerm :: CheckTerm a [Type a] (Maybe [Type a])
checkTerm a wh ctx ts term
 = contextCheckTerm ctx a wh ctx ts term


-- | Like 'checkTerm', but if the term is productive then expect
--   only a single value.
checkTerm1 :: CheckTerm a (Type a) (Maybe (Type a))
checkTerm1 a wh ctx t m
 = do   (m', rr', es') <- checkTerm a wh ctx [t] m
        case rr' of
         Nothing   -> return (m', Nothing, es')
         Just [t'] -> return (m', Just t',  es')
         Just ts'  -> throw $ ErrorWrongArityUp UTerm a wh ts' [TData]


-- | Given a list of terms, if a term is productive then expect it
--   to produce a single value of the corresponding type, where all
--   terms do not need to produce values of the same type.
checkTermsAreEach :: CheckTerms a [Type a] [Maybe (Type a)]
checkTermsAreEach a wh ctx tsExpected ms
 | length ms == length tsExpected
 = do   (ms', rss, ess')
         <- fmap unzip3
         $  zipWithM (\t m -> checkTerm1 a wh ctx t m) tsExpected ms
        return (ms', rss, concat ess')

 | otherwise
 = do   -- (_ms, ts', _ess')
        -- <- fmap unzip3 $ mapM (synthTerm1 a wh ctx) ms
        -- let ksExpected = replicate (length tsExpected) TData
        -- throw $ ErrorWrongArityUp UTerm a wh ts' ksExpected
        error "TODO: better error for arity mismatch"


-- | Given a list of terms, if a term is productive then expect it
--   to produce a single value of the given type, where all
--   terms must produce values of the same type.
checkTermsAreAll :: CheckTerms a (Type a) [Maybe (Type a)]
checkTermsAreAll a wh ctx tExpected ms
 = do   (ms', rss, effs)
         <- fmap unzip3
         $  mapM (\m -> checkTerm1 a wh ctx tExpected m) ms
        return (ms', rss, concat effs)


-- (t-check) --------------------------------------------------------------------------------------
-- | Synthesise the actual types of a term,
--   then check it against the expected types.
--   TODO: rename to synthCheckTerm or something.
checkTermHas :: CheckTerm a [Type a] (Maybe [Type a])
checkTermHas a wh ctx tsExpected m
 = do
        (m', mtsActual, esActual)
         <- synthTerm a wh ctx m

        checkResultMatchesIfDefined
                a wh ctx tsExpected mtsActual

        return (m', mtsActual, esActual)


-- | If the result is defined then check it matches the given types.
checkResultMatchesIfDefined
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> Maybe [Type a]
        -> IO ()

checkResultMatchesIfDefined
        a wh ctx tsExpected (Just tsActual)
 = do
        when (length tsActual /= length tsExpected)
         $ throw $ ErrorWrongArity UTerm a wh tsActual tsExpected

        checkTypeEquivs ctx a [] tsActual a [] tsExpected
         >>= \case
               Nothing -> return ()
               Just ((_a1, tActualErr), (_a2, tExpectedErr))
                -> throw $ ErrorMismatch UType a wh tActualErr tExpectedErr

checkResultMatchesIfDefined
        _a _wh _ctx _ Nothing
 = return ()



-- | Merge results disjunctively.
mergeResults
        :: Annot a => a -> [Where a]
        -> Context a -> [Maybe [Type a]] -> IO (Maybe [Type a])
mergeResults a wh ctx rss0
 = goNone rss0
 where
        goNone    []                = return Nothing
        goNone    (Nothing : rss)   = goNone rss
        goNone    (Just ts : rss)   = goSome ts rss

        goSome ts []                = return $ Just ts
        goSome ts (Nothing  : rss)  = goSome ts rss
        goSome ts (Just ts' : rss)
         = do
                when (length ts' /= length ts)
                   $ throw $ ErrorWrongArity UTerm a wh ts' ts

                checkTypeEquivs ctx a [] ts' a [] ts
                   >>= \case
                         Nothing -> return ()
                         Just ((_a1, tActualErr), (_a2, tExpectedErr))
                          -> throw $ ErrorMismatch UType a wh tActualErr tExpectedErr

                goSome ts rss
