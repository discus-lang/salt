
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

        -- TODO: handle alpha equivalence.
        (TVar u1, TVar u2)
         | u1 == u2
         -> Nothing

        (TRef r1, TRef r2)
         | r1 == r2
         -> Nothing

        (TAbs p1 t11, TAbs p2 t12)
         | TPTypes bks1 <- p1
         , TPTypes bks2 <- p2
         , length bks1 == length bks2
         ->  checkTypeEqs ctx a1 [] (map snd bks1) a2 [] (map snd bks2)
         <|> checkTypeEq  ctx a1 (p1 : ps1) t11    a2 (p2 : ps2) t12

        (TKey k1 gs1, TKey k2 gs2)
         | k1 == k2
         , length gs1 == length gs2
         ->  checkTypeArgsEqs t1 t2 ctx a1 ps1 gs1 a2 ps2 gs2

        (_, _)
         -> Just ((a1, t1), (a2, t2))


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

