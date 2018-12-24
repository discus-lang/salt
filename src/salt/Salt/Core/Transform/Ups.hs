
module Salt.Core.Transform.Ups where
import Salt.Core.Exp.Type
import Salt.Core.Exp.Ups
import Salt.Core.Exp.Name


-- | Apply an `Ups` to some type arguments.
upsApplyType :: Ups -> Type a -> Type a
upsApplyType ups tt
 = case tt of
        -- Descend into annotations
        TAnn a t -> TAnn a (upsApplyType ups t)

        -- Plain references don't have any variables.
        TRef{}   -> tt

        -- Apply the ups to the variable, if it matches.
        TVar u   -> TVar $ upsApplyBound ups u

        -- Carry ups under abstraction.
        TAbs tps@(TPTypes bks) tBody
         -> let nsBind  = [ n | (BindName n, _) <- bks ]
                ups'    = upsBump nsBind ups
            in  TAbs tps $ upsApplyType ups' tBody

        -- Apply ups to other types generically.
        TKey k tgss
         -> TKey k $ map (upsApplyTypeArgs ups) tgss


-- | Apply an `Ups` to some type arguments.
upsApplyTypeArgs :: Ups -> TypeArgs a -> TypeArgs a
upsApplyTypeArgs ups (TGTypes ts)
 = TGTypes $ map (upsApplyType ups) ts

