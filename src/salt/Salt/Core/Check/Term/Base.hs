
module Salt.Core.Check.Term.Base
        ( module Salt.Core.Check.Context
        , module Salt.Core.Check.Eq
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
        , checkTerm
        , checkTerm1
        , checkTermIs
        , checkTerms
        , checkTermsAreAll)
where
import Salt.Core.Check.Context
import Salt.Core.Check.Eq
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
checkTerm a wh ctx mode term
 = contextCheckTerm ctx a wh ctx mode term


-- (t-one) ----------------------------------------------------------------------------------------
-- | Like 'checkTerm' but expect a single result type.
checkTerm1
        :: Annot a => a -> [Where a]
        -> Context a -> Mode a -> Term a
        -> IO (Term a, Type a, [Effect a])

checkTerm1 a wh ctx mode m
 = do   (m', ts', es')
         <- checkTerm a wh ctx mode m
        case ts' of
         [t]    -> return (m', t, es')
         _      -> throw $ ErrorTermsWrongArity a wh ts' [TData]


-- (t-check) --------------------------------------------------------------------------------------
-- | Synthesise the actual types of a term,
--   then check it against the expected types.
checkTermIs
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> Term a
        -> IO (Term a, [Type a], [Effect a])

checkTermIs a wh ctx tsExpected m
 = do
        (m', tsActual, esActual)
         <- checkTerm a wh ctx Synth m

        when (length tsActual /= length tsExpected)
         $ throw $ ErrorTermsWrongArity a wh tsActual tsExpected

        case checkTypeEqs ctx a [] tsExpected a [] tsActual of
         Nothing -> return (m', tsActual, esActual)
         Just ((_a1, t1Err), (_a2, t2Err))
          -> throw $ ErrorTypeMismatch a wh t1Err t2Err


-- (t-many / t-gets) ------------------------------------------------------------------------------
-- This function implements both the t-many and t-gets rules from the declarative
-- version of the typing rules. In this algorithmic, bidirectional implementation
-- the expected types are represented in the checker mode we get from the caller.

-- | Check a list of individual terms.
checkTerms
        :: Annot a => a -> [Where a]
        -> Context a -> Mode a -> [Term a]
        -> IO ([Term a], [Type a], [Effect a])

checkTerms a wh ctx Synth ms
 = do   (ms', ts', ess')
         <- fmap unzip3 $ mapM (checkTerm1 a wh ctx Synth) ms
        return (ms', ts', concat ess')

checkTerms a wh ctx (Check ts) ms
 | length ms == length ts
 = do   (ms', ts', ess')
         <- fmap unzip3 $ zipWithM (\t m -> checkTerm1 a wh ctx (Check [t]) m) ts ms
        return (ms', ts', concat ess')

 | otherwise
 = do   (_ms, ts', _ess')
         <- fmap unzip3 $ mapM (checkTerm1 a wh ctx Synth) ms
        throw $ ErrorTermsWrongArity a wh ts' (replicate (length ms) TData)


-- | Check the given terms all have the specified type,
--   bundling all the caused effects together in the result.
checkTermsAreAll
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Term a]
        -> IO ([Term a], [Effect a])

checkTermsAreAll a wh ctx tExpected ms
 = do   (ms', _ts, effs)
         <- fmap unzip3 $ mapM (\m -> checkTerm1 a wh ctx (Check [tExpected]) m) ms
        return (ms', concat effs)

