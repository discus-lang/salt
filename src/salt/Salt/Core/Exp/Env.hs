
module Salt.Core.Exp.Env where
import Salt.Core.Exp.Name
import Salt.Core.Exp.Term
import Salt.Core.Exp.Type
import qualified Data.Map.Strict as Map


-- | Construct an empty environment.
envEmpty :: Env a
envEmpty = Env []


-- | Extend an environment with a new type.
envExtendType  :: Bind -> Type a -> Env a -> Env a
envExtendType bb t env@(Env evs)
 = case bb of
        BindName n      -> Env (EnvTypes (Map.singleton n t) : evs)
        BindNone        -> env


-- | Extend an environment with some new types,
--   binding them all at the same level.
envExtendTypes :: [(Bind, Type a)] -> Env a -> Env a
envExtendTypes bts1 (Env bs2)
 = let  nts     = Map.fromList [ (n, t) | (BindName n, t) <- bts1]
   in   Env (EnvTypes nts : bs2)


-- | Lookup a named type from an environment.
envLookupType :: Name -> Env a -> Maybe (Type a)
envLookupType n (Env bs0)
 = loop bs0
 where  loop []                 = Nothing
        loop (EnvValues{} : bs) = loop bs
        loop (EnvTypes nts : bs)
         = case Map.lookup n nts of
                Nothing         -> loop bs
                Just t          -> Just t


-- | Extend an environment with a new value.
envExtendValue :: Bind -> Value a -> Env a -> Env a
envExtendValue bb v env@(Env evs)
 = case bb of
        BindName n      -> Env (EnvValues (Map.singleton n v) : evs)
        BindNone        -> env


-- | Extend an environment with some new values.
envExtendValues :: [(Bind, Value a)] -> Env a -> Env a
envExtendValues bvs1 (Env bs2)
 = let  nvs     = Map.fromList [ (n, v) | (BindName n, v) <- bvs1]
   in   Env (EnvValues nvs : bs2)


