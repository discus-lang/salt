
module Salt.Core.Check.Eq where
import Salt.Core.Check.Reduce
import Salt.Core.Check.Context
import Salt.Core.Exp


---------------------------------------------------------------------------------------------------
-- Type of our checker functions.
--   We take a top-level context which holds type synonynms.
--   During comparison we walk down both type trees, mainintaining the last
--   annotation seen so far, as well as the list of type parameters we've
--   descended under.
type CheckTypeEq a x
        =  Annot a => Context a
        -> a -> [TypeParams a] -> x
        -> a -> [TypeParams a] -> x
        -> IO (Maybe ((a, Type a), (a, Type a)))


---------------------------------------------------------------------------------------------------
-- | Check that two types are equal.
--   If the types are not equal we give the inner-most annotation from both sides.
checkTypeEq :: CheckTypeEq a (Type a)
checkTypeEq ctx aL psL tL aR psR tR
 = goAnn
 where
        -- Look through annotations.
        goAnn
         | TAnn aL' tL' <- tL   = checkTypeEq ctx aL' psL tL' aR  psR tR
         | TAnn aR' tR' <- tR   = checkTypeEq ctx aL  psL tL  aR' psR tR'
         | otherwise            = goSynL

        -- Lookup whether a variable is bound as a parameter or is a synonym.
        --   If it's a synonym then we unfold it in-place.
        goSynL
         | TVar uL <- tL
         = contextResolveTypeBound ctx psL uL
         >>= \case
                Just (TypeDecl  _ tL') -> checkTypeEq ctx aL psL tL' aR psR tR
                Just (TypeParam kL lL) -> goSynR (Just (lL, kL))
                Just (TypeLocal kL lL) -> goSynR (Just (lL, kL))
                Nothing -> goSynR  Nothing

         | otherwise = goSynR Nothing

        goSynR mlkL
         | TVar uR <- tR
         = contextResolveTypeBound ctx psR uR
         >>= \case
                Just (TypeDecl  _ tR') -> checkTypeEq ctx aL psL tL  aR psR tR'
                Just (TypeParam kR lR) -> goBound mlkL (Just (lR, kR))
                Just (TypeLocal kR lR) -> goBound mlkL (Just (lR, kR))
                Nothing -> goBound mlkL Nothing

         | otherwise = goBound mlkL Nothing

        -- Compare variables based on binding level.
        --   Comparing on level instead of raw name handles alpha equivalence.
        goBound (Just (levelL, kindL)) (Just (levelR, kindR))
         | levelL == levelR
         = checkTypeEq ctx aL [] kindL aR [] kindR

        goBound Nothing Nothing = goReduceL
        goBound _ _             = goFail

        -- Reduce applications of type operators.
        goReduceL
         | TApt{}       <- tL
         = reduceType aL ctx tL
         >>= \case
                Just tL' -> checkTypeEq ctx aL psL tL' aR psR tR
                Nothing  -> goReduceR

         | otherwise    = goReduceR

        goReduceR
         | TApt{}       <- tR
         = reduceType aR ctx tR
         >>= \case
                Just tR' -> checkTypeEq ctx aL psL tL aR psR tR'
                Nothing  -> goRest

         | otherwise    = goRest

        -- Compare other types generically.
        goRest
         | TRef rL <- tL
         , TRef rR <- tR
         , rL == rR
         = return $ Nothing

         | TAbs pL@(TPTypes bksL) tBodyL <- tL
         , TAbs pR@(TPTypes bksR) tBodyR <- tR
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
altsM :: [IO (Maybe a)] -> IO (Maybe a)
altsM [] = return Nothing
altsM (c : cs)
 = do   m <- c
        case m of
         Nothing        -> altsM cs
         Just err       -> return $ Just err

