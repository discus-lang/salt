
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
        --  If this is a primitive then also determine if any effects
        --  will be caused from applying it.
        (mFun1, tFun1, esFun1)
         <- case takeMPrm mFun0 of
                Just nPrm
                 | Just pp  <- Map.lookup nPrm Prim.primOps
                 -> do  let tPrim = mapAnnot (const a) $ Prim.typeOfPrim pp
                        let ePrim = mapAnnot (const a) $ Prim.effectOfPrim pp
                        return (mFun0, tPrim, [ePrim])

                 | Just tCon <- Map.lookup nPrm Prim.primDataCtors
                 -> do  let tCon' = mapAnnot (const a) tCon
                        return (mFun0, tCon', [])

                 | otherwise
                 -> let aFun = fromMaybe a (takeAnnotOfTerm mFun0)
                    in  throw $ ErrorUnknownPrim UTerm aFun wh nPrm

                Nothing
                 -> synthTerm1 a wh ctx mFun0

        -- Check that we have at least some arguments to apply.
        when (null mgss0)
         $ throw $ ErrorAppNoArguments a wh tFun1

        -- Now that we know the type of the function we can look at the
        -- number of arguments we have and decide whether to desugar
        -- applications by reassociating them. Doing this accepts applications
        -- of functions that expect vectors to multiple term arguments,
        -- which is nicer to write in the source program.
        let elimsHave = length mgss0
        mgssReassoc
         <- if   not $ optionsReassocApps $ contextOptions ctx
            then return mgss0
            else do
                elimsNeeded   <- termElimsOfType a ctx tFun1
                if   elimsHave > elimsNeeded
                then reassocApps a ctx tFun1 mgss0
                else return mgss0

        -- Check the functional expresion against the reassociate arguments.
        (mApp', tsApp', esApp')
         <- checkTermAppArgs
                a wh ctx
                a mFun1 tFun1 mgssReassoc

        return (mApp', tsApp', esFun1 ++ esApp')


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

  -- Application of a term vector.
  goDispatch (TFun tsParam tsResult) mgssAcc mgssArg esAcc
   | (mgs : mgssRest')  <- mgssArg
   , Just (aArg, msArg) <- takeAnnMGTerms aApp mgs
   = do
        -- Check the arguments against the types of the parameters.
        (msArg', esArg)
         <- checkTerms aArg wh ctx tsParam msArg

        goHead  tsResult
                (MGAnn aArg (MGTerms msArg') : mgssAcc)
                mgssRest' (esArg ++ esAcc)

  goDispatch (TFun tsParam tsResult) mgssAcc mgssArg esAcc
   | (mgs : mgssRest')  <- mgssArg
   , Just (aArg, mArg)  <- takeAnnMGTerm aApp mgs
   = do
        -- Check the arguments against the types of the parameters.
        (mArg', esArg)
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

         TFun _ _ -> return 1

         _ -> return 0


-- | Reassociate arguments into vectors to satify the application of a
--   function with the given type.
reassocApps
        :: Annot a => a
        -> Context a -> Type a
        -> [TermArgs a] -> IO [TermArgs a]

reassocApps a ctx tFun mgss
 = do   tFun_red <- simplType a ctx tFun
        case takeTFun tFun_red of
         Nothing -> return mgss
         Just (tsParam, _tsResult)
          | length tsParam > 1
          , Just (msArg, mgss') <- takeSomeMGTerm (length tsParam) mgss
          -> return (MGTerms msArg : mgss')

          | otherwise -> return mgss


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



