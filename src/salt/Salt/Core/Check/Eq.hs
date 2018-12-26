
module Salt.Core.Check.Eq where
import Salt.Core.Check.Context
import Salt.Core.Transform.Ups
import Salt.Core.Exp
import qualified Data.Map       as Map


---------------------------------------------------------------------------------------------------
-- Type of our checker functions.
--   We take a top-level context which holds type synonynms.
--   During comparison we walk down both type trees, mainintaining the last
--   annotation seen so far, as well as the list of type parameters we've
--   descended under.
type CheckTypeEq a x
        =  Context a
        -> a -> [TypeParams a] -> x
        -> a -> [TypeParams a] -> x
        -> IO (Maybe ((a, Type a), (a, Type a)))


---------------------------------------------------------------------------------------------------
-- | Check that two types are equal.
--   If the types are not equal we give the inner-most annotation from both sides.
--
--   TODO: reduce type applications during equality checking.
--
checkTypeEq :: CheckTypeEq a (Type a)
checkTypeEq ctx aL psL tL aR psR tR
 = goAnn
 where
        -- Look through annotations.
        goAnn
         | TAnn aL' tL' <- tL
         = checkTypeEq ctx aL' psL tL' aR  psR tR

         | TAnn aR' tR' <- tR
         = checkTypeEq ctx aL  psL tL  aR' psR tR'

         | otherwise = goSynL

        -- Lookup whether a variable is bound as a parameter or is a synonym.
        --   If it's a synonym then we unfold it in-place.
        goSynL
         | TVar uL <- tL
         = resolveLevelKind ctx psL uL
         >>= \case
                Just (Left tL')  -> checkTypeEq ctx aL psL tL' aR psR tR
                Just (Right lkL) -> goSynR (Just lkL)
                Nothing          -> goSynR  Nothing

         | otherwise = goSynR Nothing

        goSynR mlkL
         | TVar uR <- tR
         = resolveLevelKind ctx psR uR
         >>= \case
                Just (Left tR')  -> checkTypeEq ctx aL psL tL  aR psR tR'
                Just (Right lkR) -> goBound mlkL (Just lkR)
                Nothing          -> goBound mlkL Nothing

         | otherwise = goBound mlkL Nothing

        -- Compare variables based on binding level.
        --   Comparing on level instead of raw name handles alpha equivalence.
        goBound (Just (levelL, kindL)) (Just (levelR, kindR))
         | levelL == levelR
         = checkTypeEq ctx aL [] kindL aR [] kindR

        goBound Nothing Nothing = goRest
        goBound _ _             = goFail

        -- Compare other types generically.
        goRest
         | TRef rL <- tL
         , TRef rR <- tR
         , rL == rR
         = return $ Nothing

         | TAbs pL tBodyL <- tL
         , TAbs pR tBodyR <- tR
         , TPTypes bksL   <- pL
         , TPTypes bksR   <- pR
         = altsM [ checkTypeEqs ctx aL [] (map snd bksL) aR [] (map snd bksR)
                 , checkTypeEq  ctx aL (pL : psL) tBodyL aR (pR : psR) tBodyR ]

         | TKey tkL tgssL <- tL
         , TKey tkR tgssR <- tR
         , tkL == tkR
         = checkTypeArgsEqs tL tR ctx aL psL tgssL aL psR tgssR

         | otherwise = goFail

        -- When the types are not equivalent report the
        -- annotation that we last descended into in both sides.
        goFail = return $ Just ((aL, tL), (aR, tR))


-- | Check that two lists are equal pairwise.
--   We require the caller to already have checked the lists are
--   of equal length.
checkTypeEqs :: CheckTypeEq a [Type a]
checkTypeEqs ctx a1 ps1 ts1 a2 ps2 ts2
 = altsM [ checkTypeEq ctx a1 ps1 t1 a2 ps2 t2
         | t1 <- ts1 | t2 <- ts2 ]


-- | Check that two type argument blocks are equal pairwise.
checkTypeArgsEq :: Type a -> Type a -> CheckTypeEq a (TypeArgs a)
checkTypeArgsEq tBlame1 tBlame2 ctx a1 ps1 g1 a2 ps2 g2
 = case (g1, g2) of
        (TGTypes ts1, TGTypes ts2)
         | length ts1 == length ts2
         -> checkTypeEqs ctx a1 ps1 ts1 a2 ps2 ts2

        _ -> return $ Just ((a1, tBlame1), (a2, tBlame2))


-- Check that two lists of type arguments are equal pairwise.
checkTypeArgsEqs :: Type a -> Type a -> CheckTypeEq a [TypeArgs a]
checkTypeArgsEqs tBlame1 tBlame2 ctx a1 ps1 gs1 a2 ps2 gs2
 = altsM [ checkTypeArgsEq tBlame1 tBlame2 ctx a1 ps1 g1 a2 ps2 g2
         | g1 <- gs1 | g2 <- gs2 ]


---------------------------------------------------------------------------------------------------
-- | Resolve a bound type variable.
--
--    We take the top level context which contains definitions of
--    type synonyms, along with the stack of parameters we've descended
--    into so far.
--
--    If the variable is bound to a synonym we get Left the definition,
--    where any variables in the type have been lifted over the binders
--    in the stack of parameters.
--
--    If the variable is bound via a parameter we get the binding level,
--    counting outwards from the current position, and the kind of the
--    binding.
--
resolveLevelKind
        :: Context a -> [TypeParams a] -> Bound
        -> IO (Maybe (Either (Type a) (Int, Kind a)))

resolveLevelKind ctx ps0 (BoundWith n d0)
 = goParams 0 d0 upsEmpty ps0
 where
        -- Look through the parameters.
        goParams level d ups (TPTypes bks : tpss)
         = case lookup (BindName n) bks of
            Nothing
             -> let ups' = upsCombine ups (upsOfBinds $ map fst bks)
                in  goParams (level + 1) d ups' tpss

            Just k
             | d == 0    -> return $ Just $ Right (level, k)
             | otherwise
             -> let ups' = upsCombine ups (upsOfBinds $ map fst bks)
                in  goParams (level + 1) (d - 1) ups' tpss

        goParams level d ups []
         = goLocal level d ups (contextLocal ctx)

        -- Look through local bindings in the context.
        goLocal level d ups (ElemTypes nks : tpss)
         | d < 0        = return Nothing
         | otherwise
         = case Map.lookup n nks of
            Nothing
             -> let ups' = upsCombine ups (upsOfNames $ Map.keys nks)
                in  goLocal level d ups' tpss

            Just k
             | d == 0    -> return $ Just $ Right (level, k)
             | otherwise
             -> let ups' = upsCombine ups (upsOfNames $ Map.keys nks)
                in  goLocal  (level + 1) (d - 1) ups' tpss

        goLocal level d ups (ElemTerms{} : tpss)
         = goLocal level d ups tpss

        goLocal _level d ups []
         | d == 0       = goGlobal ups
         | otherwise    = return $ Nothing

        -- Look for synonyms in the global context.
        goGlobal ups
         = case Map.lookup n (contextModuleType ctx) of
            Nothing      -> return Nothing
            Just (_k, t) -> return $ Just $ Left $ upsApplyType ups t


---------------------------------------------------------------------------------------------------
altsM :: [IO (Maybe a)] -> IO (Maybe a)
altsM [] = return Nothing
altsM (c : cs)
 = do   m <- c
        case m of
         Nothing        -> altsM cs
         Just err       -> return $ Just err

