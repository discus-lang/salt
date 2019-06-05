
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
        , synthTerm1, checkTerm1
        , synthTerms, checkTerms
        , checkTermHas
        , checkTermsAreAll)
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
-- | Synthesise a type for a single term.
synthTerm :: SynthTerm a
synthTerm a wh ctx term
 = contextSynthTerm ctx a wh ctx term


-- | Type check a single term.
checkTerm :: CheckTerm a
checkTerm a wh ctx ts term
 = contextCheckTerm ctx a wh ctx ts term


-- (t-one) ----------------------------------------------------------------------------------------
-- | Like 'checkTerm', but expect a single result type.
checkTerm1
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> Term a
        -> IO (Term a, [Effect a])

checkTerm1 a wh ctx t m
 = do   (m', es') <- checkTerm a wh ctx [t] m
        return (m', es')


-- | Like 'synthTerm', but expect a single result type.
synthTerm1
        :: Annot a => a -> [Where a]
        -> Context a -> Term a
        -> IO (Term a, Type a, [Effect a])

synthTerm1 a wh ctx m
 = do   (m', ts', es') <- synthTerm a wh ctx m
        case ts' of
         [t]    -> return (m', t, es')
         _      -> throw $ ErrorWrongArityUp UTerm a wh ts' [TData]


-- (t-many / t-gets) ------------------------------------------------------------------------------
-- This function implements both the t-many and t-gets rules from the declarative
-- version of the typing rules. In this algorithmic, bidirectional implementation
-- the expected types are represented in the checker mode we get from the caller.

synthTerms
        :: Annot a => a -> [Where a]
        -> Context a -> [Term a]
        -> IO ([Term a], [Type a], [Effect a])

synthTerms a wh ctx ms
 = do   (ms', ts', ess')
         <- fmap unzip3
         $  mapM (synthTerm1 a wh ctx) ms
        return (ms', ts', concat ess')


-- | Check a list of individual terms.
checkTerms
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> [Term a]
        -> IO ([Term a], [Effect a])

checkTerms a wh ctx tsExpected ms
 | length ms == length tsExpected
 = do   (ms', ess')
         <- fmap unzip
         $  zipWithM (\t m -> checkTerm1 a wh ctx t m) tsExpected ms
        return (ms', concat ess')

 | otherwise
 = do   (_ms, ts', _ess')
         <- fmap unzip3 $ mapM (synthTerm1 a wh ctx) ms
        let ksExpected = replicate (length tsExpected) TData
        throw $ ErrorWrongArityUp UTerm a wh ts' ksExpected


-- | Check the given terms all have the specified type,
--   bundling all the caused effects together in the result.
checkTermsAreAll
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Term a]
        -> IO ([Term a], [Effect a])

checkTermsAreAll a wh ctx tExpected ms
 = do   (ms', effs)
         <- fmap unzip
         $  mapM (\m -> checkTerm1 a wh ctx tExpected m) ms
        return (ms', concat effs)


-- (t-check) --------------------------------------------------------------------------------------
-- | Synthesise the actual types of a term,
--   then check it against the expected types.
--   TODO: rename to synthCheckTerm or something.
checkTermHas
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> Term a
        -> IO (Term a, [Effect a])

checkTermHas a wh ctx tsExpected m
 = do
        (m', tsActual, esActual)
         <- synthTerm a wh ctx m

        when (length tsActual /= length tsExpected)
         $ throw $ ErrorWrongArity UTerm a wh tsActual tsExpected

        checkTypeEquivs ctx a [] tsActual a [] tsExpected
         >>= \case
                Nothing -> return (m', esActual)
                Just ((_a1, tActualErr), (_a2, tExpectedErr))
                 -> throw $ ErrorMismatch UType a wh tActualErr tExpectedErr
