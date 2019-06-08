
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
        , synthTerm,  checkTerm
        , synthTermProductive, synthTermProductive1
        , synthTermsConjunctive
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


---------------------------------------------------------------------------------------------------
-- | Type check a single term.
checkTerm :: CheckTerm a
checkTerm a wh ctx ts term
 = contextCheckTerm ctx a wh ctx ts term


-- | Synthesise a type for a single term.
synthTerm :: SynthTerm a
synthTerm a wh ctx term
 = contextSynthTerm ctx a wh ctx term


-- | Like `synthTerm`, but expect the term to be productive.
synthTermProductive
        :: Annot a => a -> [Where a]
        -> Context a -> Term a
        -> IO (Term a, [Type a], [Effect a])

synthTermProductive a wh ctx m
 = do   (m', rr', es') <- synthTerm a wh ctx m
        case rr' of
         Nothing  -> error "TODO: not productive. add better error message."
         Just ts' -> return (m', ts', es')


-- (t-one) ----------------------------------------------------------------------------------------
-- | Like 'checkTerm', but expect a single result type.
checkTerm1
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> Term a
        -> IO (Term a, Maybe (Type a), [Effect a])

checkTerm1 a wh ctx t m
 = do   (m', rr', es') <- checkTerm a wh ctx [t] m
        case rr' of
         Nothing   -> return (m', Nothing, es')
         Just [t'] -> return (m', Just t',  es')
         Just ts'  -> throw $ ErrorWrongArityUp UTerm a wh ts' [TData]


-- | Like 'synthTerm1', but expect the term to be productive.
synthTermProductive1
        :: Annot a => a -> [Where a]
        -> Context a -> Term a
        -> IO (Term a, Type a, [Effect a])

synthTermProductive1 a wh ctx m
 = do   (m', rr', es') <- synthTerm a wh ctx m
        case rr' of
         Nothing  -> error "TODO: not productive. add better error message."
         Just [t] -> return (m', t, es')
         Just ts' -> throw $ ErrorWrongArityUp UTerm a wh ts' [TData]


-- (t-many / t-gets) ------------------------------------------------------------------------------

-- Synthesise types for each of the terms,
-- and combine them conjunctively into the same result vector.
synthTermsConjunctive
        :: Annot a => a -> [Where a]
        -> Context a -> [Term a]
        -> IO ([Term a], Maybe [Type a], [Effect a])

synthTermsConjunctive a wh ctx ms
 = do   (ms', rss', ess')
         <- fmap unzip3 $ mapM (synthTerm a wh ctx) ms

        let rs' = fmap concat $ sequence rss'

        return (ms', rs', concat ess')


-- | Given a list of terms, if a term's result is defined then check
--   that it matches the corresponding type vector.
checkTermsAreEach
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> [Term a]
        -> IO ([Term a], [Effect a])

checkTermsAreEach a wh ctx tsExpected ms
 | length ms == length tsExpected
 = do   (ms', _rs, ess')
         <- fmap unzip3
         $  zipWithM (\t m -> checkTerm1 a wh ctx t m) tsExpected ms
        return (ms', concat ess')

 | otherwise
 = do   -- (_ms, ts', _ess')
        -- <- fmap unzip3 $ mapM (synthTerm1 a wh ctx) ms
        -- let ksExpected = replicate (length tsExpected) TData
        -- throw $ ErrorWrongArityUp UTerm a wh ts' ksExpected
        error "TODO: better error for arity mismatch"


-- Like `checkTermsAreEach`, but use the same expected type for each
-- of the terms.
checkTermsAreAll
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Term a]
        -> IO ([Term a], [Effect a])

checkTermsAreAll a wh ctx tExpected ms
 = do   (ms', _rs, effs)
         <- fmap unzip3
         $  mapM (\m -> checkTerm1 a wh ctx tExpected m) ms
        return (ms', concat effs)


-- (t-check) --------------------------------------------------------------------------------------
-- | Synthesise the actual types of a term,
--   then check it against the expected types.
--   TODO: rename to synthCheckTerm or something.
checkTermHas
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> Term a
        -> IO (Term a, Maybe [Type a], [Effect a])

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

