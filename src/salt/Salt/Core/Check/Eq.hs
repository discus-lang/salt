
module Salt.Core.Check.Eq where
import Salt.Core.Check.Context
import Salt.Core.Exp
import Control.Applicative
import qualified Data.Map       as Map


type CheckTypeEq a x
        =  Context a
        -> a -> [TypeParams a] -> x
        -> a -> [TypeParams a] -> x
        -> Maybe ((a, Type a), (a, Type a))


-- | Check that two types are equal.
--   If the types are not equal we give the inner-most annotation from
--   both sides.
checkTypeEq :: CheckTypeEq a (Type a)
checkTypeEq ctx a1 ps1 t1 a2 ps2 t2
 = case (t1, t2) of
        (TAnn a1' t1', _)
         -> checkTypeEq ctx a1' ps1 t1' a2 ps2 t2

        (_, TAnn a2' t2')
         -> checkTypeEq ctx a1 ps1 t1 a2' ps2 t2'

        -- TODO: avoid variable capture.
        -- We need to bump the bound type past all the quantifiers we've
        -- currently underneath.
        (TVar (Bound n1), _)
         |  Just (_k, t1') <- Map.lookup n1 (contextModuleType ctx)
         -> checkTypeEq ctx a1 ps1 t1' a2 ps2 t2

        (_, TVar (Bound n2))
         |  Just (_k, t2') <- Map.lookup n2 (contextModuleType ctx)
         -> checkTypeEq ctx a1 ps1 t1  a2 ps2 t2'

        -- variables.
        -- To handle differences in naming we need to check where the
        -- variable was bound, not the syntactic name.
        (TVar (Bound u1), TVar (Bound u2))
         | Just (l1, k1) <- levelKindOfTyVar ctx ps1 u1
         , Just (l2, k2) <- levelKindOfTyVar ctx ps2 u2
         , l1 == l2
         , Nothing       <- checkTypeEq ctx a1 [] k1 a2 [] k2
         -> Nothing

        -- references
        (TRef r1, TRef r2)
         | r1 == r2
         -> Nothing

        -- abstractions
        (TAbs p1 t11, TAbs p2 t12)
         | TPTypes bks1 <- p1
         , TPTypes bks2 <- p2
         , length bks1 == length bks2
         ->  checkTypeEqs ctx a1 [] (map snd bks1) a2 [] (map snd bks2)
         <|> checkTypeEq  ctx a1 (p1 : ps1) t11    a2 (p2 : ps2) t12

        -- type keys
        (TKey k1 gs1, TKey k2 gs2)
         | k1 == k2
         , length gs1 == length gs2
         ->  checkTypeArgsEqs t1 t2 ctx a1 ps1 gs1 a2 ps2 gs2

        (_, _)
         -> Just ((a1, t1), (a2, t2))


-- | Lookup the binding level and kind of a type variable name.
--   The binding level is the number of enclosing type abstractions
--   in the context between the current position and the binder
--   of the variable.
levelKindOfTyVar :: Context a -> [TypeParams a] -> Name -> Maybe (Int, Kind a)
levelKindOfTyVar ctx tps0 n
 = goParams 0 tps0
 where
        goParams level []
         = goLocal level (contextLocal ctx)

        goParams level (TPTypes bks : tps)
         = case lookup (BindName n) bks of
                Nothing -> goParams (level + 1) tps
                Just k  -> Just (level, k)

        goLocal _level []
         = Nothing

        goLocal level (ElemTypes mps : tps)
         = case Map.lookup n mps of
                Nothing -> goLocal (level + 1) tps
                Just k  -> Just (level, k)

        goLocal level (ElemTerms{} : tps)
         = goLocal (level + 1) tps


-- | Check that two lists are equal pairwise.
--   We require the caller to already have checked the lists are
--   of equal length.
checkTypeEqs :: CheckTypeEq a [Type a]
checkTypeEqs ctx a1 ps1 ts1 a2 ps2 ts2
 = foldl (<|>) Nothing
        [ checkTypeEq ctx a1 ps1 t1 a2 ps2 t2
        | t1 <- ts1 | t2 <- ts2 ]


-- | Check that two type argument blocks are equal pairwise.
checkTypeArgsEq :: Type a -> Type a -> CheckTypeEq a (TypeArgs a)
checkTypeArgsEq tBlame1 tBlame2 ctx a1 ps1 g1 a2 ps2 g2
 = case (g1, g2) of
        (TGTypes ts1, TGTypes ts2)
         | length ts1 == length ts2
         -> checkTypeEqs ctx a1 ps1 ts1 a2 ps2 ts2

        _ -> Just ((a1, tBlame1), (a2, tBlame2))


-- Check that two lists of type arguments are equal pairwise.
checkTypeArgsEqs :: Type a -> Type a -> CheckTypeEq a [TypeArgs a]
checkTypeArgsEqs tBlame1 tBlame2 ctx a1 ps1 gs1 a2 ps2 gs2
 = foldl (<|>) Nothing
        [ checkTypeArgsEq tBlame1 tBlame2 ctx a1 ps1 g1 a2 ps2 g2
        | g1 <- gs1 | g2 <- gs2 ]

