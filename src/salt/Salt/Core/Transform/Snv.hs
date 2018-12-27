
module Salt.Core.Transform.Snv where
import Salt.Core.Transform.Ups
import Salt.Core.Exp.Snv
import Salt.Core.Exp
import qualified Data.Map       as Map


-- | Apply a type substitution to a type.
--   As we carry the subtitution into the tree
snvApplyType :: Ups -> Snv (Type a) -> Type a -> Type a
snvApplyType ups snv tt
 = case tt of
        -- Decend into annotations.
        TAnn a t -> TAnn a (snvApplyType ups snv t)

        -- Plain references don't have any variables.
        TRef{}   -> tt

        -- Apply substitution to the variable.
        TVar u
         -> case snvApplyBound snv u of
                Left u'  -> TVar u'
                Right t' -> upsApplyType ups t'

        -- Carry subsitution under binders.
        --   We update the ups to account for any binders that shadow
        --   elements of our subsitution.
        TAbs tps@(TPTypes bks) tBody
         -> let nsBind  = [ n | (BindName n, _) <- bks ]
                upsBind = upsOfNames nsBind
                ups'    = upsCombine upsBind ups
                snv'    = snvBump nsBind snv
            in  TAbs tps $ snvApplyType ups' snv' tBody

        -- Apply substitution to other types generically.
        TKey k tgss
         -> TKey k $ map (snvApplyTypeArgs ups snv) tgss


-- | Apply a type substitution to some type arguments.
snvApplyTypeArgs :: Ups -> Snv (Type a) -> TypeArgs a -> TypeArgs a
snvApplyTypeArgs ups snv (TGTypes ts)
 = TGTypes $ map (snvApplyType ups snv) ts


snvOfEnvTypes :: Env a -> Snv (Type a)
snvOfEnvTypes (Env bs)
 = Snv $ concatMap takeBinds bs
 where
        takeBinds (EnvTypes mp)
         = [ ((n, 0), t) | (n, t) <- Map.toList mp ]

        takeBinds (EnvValues{})
         = []

