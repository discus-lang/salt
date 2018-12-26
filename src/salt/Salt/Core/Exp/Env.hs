
module Salt.Core.Exp.Env where
import Salt.Core.Exp.Name
import Salt.Core.Exp.Term
import Salt.Core.Exp.Type


-- | Construct an empty environment.
envEmpty :: Env a
envEmpty = Env []


-- | Extend an environment with a new type.
envExtendType  :: Bind -> Type a -> Env a -> Env a
envExtendType bb t env@(Env evs)
 = case bb of
        BindName n      -> Env (EnvType n t : evs)
        BindNone        -> env


-- | Extend an environment with some new types.
envExtendsType :: [(Bind, Type a)] -> Env a -> Env a
envExtendsType bts1 (Env bs2)
        = Env $ [EnvType n t | (BindName n, t) <- bts1] ++ bs2


-- | Lookup a named type from an environment.
envLookupType :: Name -> Env a -> Maybe (Type a)
envLookupType n (Env bs0)
 = loop bs0
 where  loop []                 = Nothing
        loop (EnvValue{} : bs)  = loop bs
        loop (EnvType n' t : bs)
         | n == n'      = Just t
         | otherwise    = loop bs


-- | Extend an environment with a new value.
envExtendValue :: Bind -> Value a -> Env a -> Env a
envExtendValue bb v env@(Env evs)
 = case bb of
        BindName n      -> Env (EnvValue n v : evs)
        BindNone        -> env


-- | Extend an environment with some new values.
envExtendsValue :: [(Bind, Value a)] -> Env a -> Env a
envExtendsValue bvs1 (Env bs2)
        = Env $ [EnvValue n v | (BindName n, v) <- bvs1] ++ bs2


-- | Lookup a named value from an environment.
envLookupValue :: Name -> Env a -> Maybe (Value a)
envLookupValue n (Env bs0)
 = loop bs0
 where  loop []                 = Nothing
        loop (EnvType{} : bs)   = loop bs
        loop (EnvValue n' v : bs)
         | n' == n      = Just v
         | otherwise    = loop bs
