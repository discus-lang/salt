
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


-- | Extend an environment with some new types.
envExtendsType :: [(Bind, Type a)] -> Env a -> Env a
envExtendsType bts1 (Env bs2)
 = Env (EnvTypes (Map.fromList [ (n, t) | (BindName n, t) <- bts1]) : bs2)


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
envExtendsValue :: [(Bind, Value a)] -> Env a -> Env a
envExtendsValue bvs1 (Env bs2)
 = Env (EnvValues (Map.fromList [ (n, v) | (BindName n, v) <- bvs1]) : bs2)


-- | Lookup a named value from an environment.
envLookupValue :: Name -> Env a -> Maybe (Value a)
envLookupValue n (Env bs0)
 = loop bs0
 where  loop []                 = Nothing
        loop (EnvTypes{} : bs)  = loop bs
        loop (EnvValues nvs : bs)
         = case Map.lookup n nvs of
                Nothing         -> loop bs
                Just t          -> Just t

