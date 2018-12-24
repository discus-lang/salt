
module Salt.Core.Transform.Subst where
import Salt.Core.Exp
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map


-- | Substitution of types in types.
substTypeType :: Map Name (Type a) -> Type a -> Type a
substTypeType subst tt
 = case tt of
        TAnn a t -> TAnn a (substTypeType subst t)
        TRef{}   -> tt

        TVar (BoundWith n 0)
         -> case Map.lookup n subst of
                Nothing -> tt
                Just t  -> t

        TVar{} -> error "handle bumps in substTypeType"

        -- TODO: lift types in subst across bindings.
        TAbs gs t
         -> TAbs gs (substTypeType subst t)

        TKey k tgs
         -> TKey k (map (substTypeTypeArgs subst) tgs)


-- | Substitution of types into type arguments.
substTypeTypeArgs :: Map Name (Type a) -> TypeArgs a -> TypeArgs a
substTypeTypeArgs subst gs
 = case gs of
        TGTypes ts      -> TGTypes (map (substTypeType subst) ts)
