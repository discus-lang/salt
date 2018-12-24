
module Salt.Core.Transform.Push where
import Salt.Core.Exp.Type
import Salt.Core.Exp.Name


-- | Push down any outer-most Ups nodes to the leaves of the type.
--   We also push outer-most Ups that are wrapped in annotations.
pushUpsOfType :: Type a -> Type a
pushUpsOfType tt
 = case tt of
        -- Look through annotations.
        TAnn a t
         -> TAnn a (pushUpsOfType t)

        -- Lift out annotations so we can see what the bump is applied to.
        TUps upsT (TAnn a t)
         -> pushUpsOfType $ TAnn a (TUps upsT t)

        -- Plain references don't have any variables.
        TUps _upsT t@TRef{}
         -> t

        -- Apply the ups to the variable, if it matches.
        TUps upsT (TVar u)
         -> TVar $ upsApplyBound upsT u

        -- Push ups under abstraction,
        -- increasing the depth of bumps that have the same name
        -- as any of the binders.
        TUps upsT (TAbs tps@(TPTypes bks) tBody)
         -> let nsBind  = [ n | (BindName n, _) <- bks ]
                upsT'   = upsBump nsBind upsT
            in  TAbs tps $ pushUpsOfType (TUps upsT' tBody)

        -- Combine multiple ups into a single one,
        -- to simplity the type and reduce the cost of the traversal.
        TUps upsT1 (TUps upsT2 t)
         -> pushUpsOfType $ TUps (upsCombine upsT1 upsT2) t

        -- Apply ups to other types generically.
        TUps upsT (TKey k tgss)
         -> TKey k [TGTypes (map (pushUpsOfType . TUps upsT) ts) | TGTypes ts <- tgss]

        -- We only push down outermost Ups.
        TRef{} -> tt
        TVar{} -> tt
        TAbs{} -> tt
        TKey{} -> tt

