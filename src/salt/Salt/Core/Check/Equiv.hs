
module Salt.Core.Check.Equiv where
import Salt.Core.Check.Reduce
import Salt.Core.Check.Context
import Salt.Core.Exp


---------------------------------------------------------------------------------------------------
-- Type of our equivalence checker functions.
--   We take a top-level context which holds type synonyms.
--   During comparison we walk down both type trees, maintaining the last
--   annotation seen so far, as well as the list of type parameters we've
--   descended under.
type CheckTypeEquiv a x
        =  Annot a => Context a
        -> a -> [TypeParams a] -> x
        -> a -> [TypeParams a] -> x
        -> IO (Maybe ((a, Type a), (a, Type a)))


---------------------------------------------------------------------------------------------------
-- | Check that two types are equivalent.
--
--   If the types are not equivalent we give the inner-most annotation from
--   both sides.
checkTypeEquiv :: CheckTypeEquiv a (Type a)
checkTypeEquiv ctx aL psL tL aR psR tR
 = goNormalize
 where
        -- Normalize both sides before comparing them.
        goNormalize
         | TAnn aL' tL' <- tL
         = checkTypeEquiv ctx aL' psL tL' aR psR tR

         | TAnn aR' tR' <- tR
         = checkTypeEquiv ctx aL  psL tL aR' psR tR'

         | otherwise
         = do   etL <- normalize aL psL tL
                etR <- normalize aL psR tR
                goCompare etL etR

        -- Compare normalized types.
        goCompare etL etR
         -- Variables that are bound at a given level.
         | Left  (aL', kL, lL) <- etL
         , Left  (aR', kR, lR) <- etR
         , lL == lR
         = checkTypeEquiv ctx aL' [] kL aR' [] kR

         -- References.
         | Right (_aL', TRef rL) <- etL
         , Right (_aR', TRef rR) <- etR
         = case (rL, rR) of
                (TRPrm nL, TRPrm nR)
                 | nL == nR     -> return Nothing

                (TRCon nL, TRCon nR)
                 | nL == nR     -> return Nothing

                -- No part of the system currently needs to compare type closures.
                _ -> return $ Just ((aL, tL), (aR, tR))

         -- Abstractions.
         | Right (aL', TAbs tpsL tBodyL) <- etL
         , Right (aR', TAbs tpsR tBodyR) <- etR
         , bksL <- takeTPTypes tpsL
         , bksR <- takeTPTypes tpsR
         = altsM [ checkTypeEquivs ctx aL' [] (map snd bksL) aR' [] (map snd bksR)
                 , checkTypeEquiv  ctx aL' (tpsL : psL) tBodyL aR' (tpsR : psR) tBodyR ]

         -- Compare other types generically.
         | Right (aL', TKey tkL tgssL) <- etL
         , Right (aR', TKey tkR tgssR) <- etR
         , tkL == tkR
         = checkTypeArgsEquivs tL tR ctx aL' psL tgssL aR' psR tgssR

         -- These types are not equivalent.
         | otherwise
         = return $ Just ((aL, tL), (aR, tR))

        -- Normalize a type in preparation for comparison.
        --   We look through annotations, unfold type declarations in head
        --   position and reduce applications of type operators.
        normalize a ps tt
         | TAnn a' t' <- tt
         = normalize a' ps t'

         | TVar u <- tt
         = contextResolveTypeBound ctx ps u
         >>= \case
                Just (TypeDecl  _ t')   -> normalize a ps t'
                Just (TypeParam k l)    -> return $ Left  (a, k, l)
                Just (TypeLocal k l)    -> return $ Left  (a, k, l)
                Nothing                 -> return $ Right (a, tt)

         | TApp{} <- tt
         = reduceType a ctx tt
         >>= \case
                Just t' -> normalize a ps t'
                Nothing -> return $ Right (a, tt)

         | TSum []   <- tt
         = normalize a ps TPure

         | TSum [t'] <- tt
         = normalize a ps t'

         | TSum{} <- tt
         = case flattenType tt of
                Just t'          -> normalize a ps t'
                Nothing          -> return $ Right (a, tt)

         | otherwise
         = return $ Right (a, tt)


-- | Check that two lists are equivalent pairwise.
--   We require the caller to already have checked the lists are
--   of equal length.
checkTypeEquivs :: CheckTypeEquiv a [Type a]
checkTypeEquivs ctx a1 ps1 ts1 a2 ps2 ts2
 = altsM [ checkTypeEquiv ctx a1 ps1 t1 a2 ps2 t2
         | t1 <- ts1 | t2 <- ts2 ]


-- | Check that two type argument blocks are equivalent pairwise.
checkTypeArgsEquiv :: Type a -> Type a -> CheckTypeEquiv a (TypeArgs a)
checkTypeArgsEquiv tBlame1 tBlame2 ctx a1 ps1 tgs1 a2 ps2 tgs2
 = case (tgs1, tgs2) of
        (TGAnn _ tgs1', _)
         -> checkTypeArgsEquiv tBlame1 tBlame2 ctx a1 ps1 tgs1' a2 ps2 tgs2

        (_, TGAnn _ tgs2')
         -> checkTypeArgsEquiv tBlame1 tBlame2 ctx a1 ps1 tgs1  a2 ps2 tgs2'

        (TGTypes ts1, TGTypes ts2)
         | length ts1 == length ts2
         -> checkTypeEquivs ctx a1 ps1 ts1 a2 ps2 ts2

        _ -> return $ Just ((a1, tBlame1), (a2, tBlame2))


-- | Check that two lists of type arguments are equivalent pairwise.
checkTypeArgsEquivs :: Type a -> Type a -> CheckTypeEquiv a [TypeArgs a]
checkTypeArgsEquivs tBlame1 tBlame2 ctx a1 ps1 gs1 a2 ps2 gs2
 = altsM [ checkTypeArgsEquiv tBlame1 tBlame2 ctx a1 ps1 g1 a2 ps2 g2
         | g1 <- gs1 | g2 <- gs2 ]


-- | Evaluate a list of monadic actions that return `Maybe` things.
--   We take actions that return `Nothing` as successful, but stop and return
--   the result of the first one that produces a `Just` thing.
altsM :: [IO (Maybe a)] -> IO (Maybe a)
altsM [] = return Nothing
altsM (c : cs)
 = do   m <- c
        case m of
         Nothing        -> altsM cs
         Just err       -> return $ Just err

