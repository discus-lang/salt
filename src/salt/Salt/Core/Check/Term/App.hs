
module Salt.Core.Check.Term.App where
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import qualified Salt.Core.Prim.Ops     as Prim
import qualified Salt.Core.Prim.Ctor    as Prim
import qualified Data.Map.Strict        as Map


-------------------------------------------------------------------------------------------- App --
-- | Check an application of a term to its arguments.
checkTermApp
        :: Annot a => a -> [Where a]
        -> Context a -> Term a -> [TermArgs a]
        -> IO (Term a, [Type a], [Effect a])

checkTermApp a wh ctx mFun0 mgss0
 = do
        -- Determine the type of the functional expression.
        (mFun1, tFun1, esFun1)
         <- case takeMPrm mFun0 of
                Just nPrm
                 | Just pp  <- Map.lookup nPrm Prim.primOps
                 -> do  let tPrim = mapAnnot (const a) $ Prim.typeOfPrim pp
                        return (mFun0, tPrim, [])

                 | Just tCon <- Map.lookup nPrm Prim.primDataCtors
                 -> do  let tCon' = mapAnnot (const a) tCon
                        return (mFun0, tCon', [])

                 | otherwise
                 -> let aFun = fromMaybe a (takeAnnotOfTerm mFun0)
                    in  throw $ ErrorUnknownPrim UTerm aFun wh nPrm

                Nothing
                 -> synthTermProductive1 a wh ctx mFun0

        -- Check that we have at least some arguments to apply.
        when (null mgss0)
         $ throw $ ErrorAppNoArguments a wh tFun1

        -- Now that we know the type of the function we can look at the
        -- number of arguments we have and decide whether to desugar
        -- applications by reassociating them. Doing this lets us apply
        -- functions that expect vectors of arguments to just a sufficient
        -- number of individual arguments, which is nicer to write in
        -- the source program.
        let elimsHave = length mgss0
        mgssReassoc
         <- if   not $ optionsReassocApps $ contextOptions ctx
            then return mgss0
            else do
                elimsNeeded   <- termElimsOfType a ctx tFun1
                if   elimsHave > elimsNeeded
                then reassocApps a ctx tFun1 mgss0
                else return mgss0

        -- Check the functional expresion against the reassociated arguments.
        (mApp', tsApp_result, esApp')
         <- checkTermAppArgs
                a wh ctx
                a mFun1 tFun1 mgssReassoc

        -- If the result is a suspension then automatically run it.
        --   Suspension types have kind Comp rather than Data,
        --     so if we do not run them here then the result would be ill-kinded.
        (tsApp_run, esRun)
         <- case tsApp_result of
                [t] ->  simplType a ctx t
                    >>= \case
                         TSusp tsv te   -> return (tsv, [te])
                         _              -> return ([t], [])
                _   -> return (tsApp_result, [])

        return  ( mApp'
                , tsApp_run
                , esFun1 ++ esApp' ++ esRun)


--------------------------------------------------------------------------------------- App/Args --
checkTermAppArgs
        :: Annot a
        => a -> [Where a] -> Context a
        -> a -> Term a -> Type a -> [TermArgs a]
        -> IO (Term a, [Type a], [Effect a])

checkTermAppArgs aApp wh ctx aFun mFun tFun mgssArg0
 = goHead [tFun] [] mgssArg0 []
 where
  goHead tsHead mgssAcc [] esAcc
   = do let mApp = MAnn aApp $ MAps mFun (reverse mgssAcc)
        return  ( mApp
                , tsHead, esAcc)

  goHead [tHead] mgssAcc mgssArg esAcc
   = do -- Check the head of the functional type to determine
        -- what sort of arguments to expect.
        tFun_red <- simplType aFun ctx tHead
        goDispatch tFun_red mgssAcc mgssArg esAcc

  goHead tsHead _ _ _
   = throw $ ErrorAppVector aApp wh tsHead

  -- Function expects some type arguments.
  goDispatch (TForall tpsParam tResult) mgssAcc mgssArg esAcc
   | (mgs : mgssRest)   <- mgssArg
   , Just (aArg, tsArg) <- takeAnnMGTypes aApp mgs
   = do
        let bksParam = takeTPTypes tpsParam

        -- Check the kinds of the arguments.
        (tsArg', ksArg)
         <- checkTypes aArg wh ctx tsArg

        -- The number of arguments must match the number of parameters.
        when (not $ length bksParam == length tsArg)
         $ throw $ ErrorAppTermTypeWrongArity aArg wh bksParam tsArg

        -- Check the parameter and argument kinds match.
        (checkTypeEquivs ctx aApp [] (map snd bksParam) aApp [] ksArg
         >>= \case
                Nothing -> return ()
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                  -> throw $ ErrorMismatch UType aApp wh tErr1 tErr2)

        -- Substitute arguments into the result type to instantiate
        -- the type scheme.
        let nts    = [ (n, t) | (BindName n, _k) <- bksParam | t <- tsArg ]
        let snv    = snvOfBinds nts
        let tSubst = snvApplyType upsEmpty snv tResult

        -- Continue collecting arguments.
        goHead  [tSubst]
                (MGAnn aArg (MGTypes tsArg') : mgssAcc)
                mgssRest esAcc

   | otherwise = throw $ ErrorAppTermTypeCannot aApp wh tFun

  -- Function expects a term vector.
  goDispatch (TFun tsParam tsResult) mgssAcc mgssArg esAcc
   | (mgs : mgssRest')  <- mgssArg
   , Just (aArg, msArg) <- takeAnnMGTerms aApp mgs
   = do
        -- Check the arguments against the types of the parameters.
        (msArg', _rsArg, esArg)
         <- checkTermsAreEach aArg wh ctx tsParam msArg

        goHead  tsResult
                (MGAnn aArg (MGTerms msArg') : mgssAcc)
                mgssRest' (esArg ++ esAcc)

  -- Function expects a single term.
  goDispatch (TFun tsParam tsResult) mgssAcc mgssArg esAcc
   | (mgs : mgssRest')  <- mgssArg
   , Just (aArg, mArg)  <- takeAnnMGTerm aApp mgs
   = do
        -- Check the arguments against the types of the parameters.
        (mArg', _rr, esArg)
         <- checkTerm aArg wh ctx tsParam mArg

        goHead  tsResult
                (MGAnn aArg (MGTerm mArg') : mgssAcc)
                mgssRest' (esArg ++ esAcc)

   | otherwise = throw $ ErrorAppTermTermCannot aApp wh tFun

  goDispatch tHead _mgssAcc _mgssArg _esAcc
   = throw $ ErrorAppNotFunction aApp wh tHead


---------------------------------------------------------------------------------------------------
-- | Given the type of a function, determine the number of applications
--   that would be needed to fully apply it. If we apply it to any more
--   arguments than this then the function would definately be ill-typed.
termElimsOfType
        :: Annot a => a
        -> Context a -> Type a -> IO Int

termElimsOfType a ctx tFun
 = do   tFun_red <- simplType a ctx tFun
        case tFun_red of
         TFun _ [tResult]
          -> do n' <- termElimsOfType a ctx tResult
                return $ 1 + n'

         TFun _ _   -> return 1

         TForall _ tResult
          -> do n' <- termElimsOfType a ctx tResult
                return $ 1 + n'

         _ -> return 0


-- | Reassociate arguments into vectors to satify the application of a
--   function with the given type.
reassocApps
        :: Annot a => a
        -> Context a -> Type a
        -> [TermArgs a] -> IO [TermArgs a]

reassocApps a ctx tFun mgss
 = do   tFun_red <- simplType a ctx tFun
        case tFun_red of
         -- Application of a term function.
         TFun tsParam tsResult
          | length tsParam >= 0
          , Just (msArg, mgss') <- takeSomeMGTerm (length tsParam) mgss
          -> case tsResult of
                -- If the function result is another function
                -- then continue trying to reassociate the application.
                [tResult]
                  -> do mgss'' <- reassocApps a ctx tResult mgss'
                        return (MGTerms msArg : mgss'')

                -- If there are no results at all then go with the
                -- current reassociation.
                _ -> do return (MGTerms msArg : mgss')

         -- Application of a type function.
         TForall (TPTypes bks) tResult
          | length bks >= 0
          , Just (tsArg, mgss') <- takeSomeMGType (length bks) mgss
          -> do mgss'' <- reassocApps a ctx tResult mgss'
                return (MGTypes tsArg : mgss'')

         -- This looks ill-typed, so don't try to reassociate it.
         -- The rest of the type checker will find the problem.
         _ -> return mgss


-- | Try to take the given number of MGTerm arguments from the front
--   of an arguments list.
--   TODO: preserve annotations.
takeSomeMGTerm :: Int -> [TermArgs a] -> Maybe ([Term a], [TermArgs a])
takeSomeMGTerm 0 mgss
 = (Just ([], mgss))

takeSomeMGTerm n (MGAnn _a mgs : mgss)
 = takeSomeMGTerm n (mgs : mgss)

takeSomeMGTerm n (MGTerm m : mgss)
 = case takeSomeMGTerm (n - 1) mgss of
        Nothing          -> Nothing
        Just (ms, mgss') -> Just (m : ms, mgss')

takeSomeMGTerm _ _ = Nothing


-- | Try to take the given number of MGType arguments from the front
--   of an arguments list.
--   TODO: preserve annotations.
takeSomeMGType :: Int -> [TermArgs a] -> Maybe ([Type a], [TermArgs a])
takeSomeMGType 0 mgss
 = (Just ([], mgss))

takeSomeMGType n (MGAnn _a mgs : mgss)
 = takeSomeMGType n (mgs : mgss)

takeSomeMGType n (MGTypes [t] : mgss)
 = case takeSomeMGType (n - 1) mgss of
        Nothing          -> Nothing
        Just (ts, mgss') -> Just (t : ts, mgss')

takeSomeMGType _n mgss
 = (Just ([], mgss))
