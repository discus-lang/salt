
module Salt.Core.Transform.Subst where
import Salt.Core.Exp
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map


-- | Substitution of types in types.
substTypeType :: [Map Name (Type a)] -> Type a -> Type a
substTypeType subst tt
 = case tt of
        TAnn a t        -> TAnn a (substTypeType subst t)
        TRef{}          -> tt

        TVar (Bound n)
         -> case goLookup n subst of
                Nothing -> tt
                Just t' -> t'

        -- TODO: lift types in subst across bindings.
        TAbs gs t
         -> TAbs gs (substTypeType subst t)

        TKey k tgs
         -> TKey k (map (substTypeTypeArgs subst) tgs)

 where
        goLookup _n []    = Nothing
        goLookup n (mp : mpMore)
         = case Map.lookup n mp of
                Nothing -> goLookup n mpMore
                Just t  -> Just t


-- | Substitution of types into type arguments.
substTypeTypeArgs :: [Map Name (Type a)] -> TypeArgs a -> TypeArgs a
substTypeTypeArgs subst gs
 = case gs of
        TGTypes ts      -> TGTypes (map (substTypeType subst) ts)
