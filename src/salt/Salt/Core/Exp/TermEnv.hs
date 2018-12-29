
module Salt.Core.Exp.TermEnv where
import Salt.Core.Exp.Name
import Salt.Core.Exp.Term
import Salt.Core.Exp.Type
import qualified Data.Map.Strict as Map


-- | Construct an empty term environment.
menvEmpty :: TermEnv a
menvEmpty = TermEnv []


-- | Extend a term environment with a new type.
menvExtendType  :: Bind -> Type a -> TermEnv a -> TermEnv a
menvExtendType bb t env@(TermEnv evs)
 = case bb of
        BindName n      -> TermEnv (TermEnvTypes (Map.singleton n t) : evs)
        BindNone        -> env


-- | Extend an environment with some new types,
--   binding them all at the same level.
menvExtendTypes :: [(Bind, Type a)] -> TermEnv a -> TermEnv a
menvExtendTypes bts1 (TermEnv bs2)
 = let  nts     = Map.fromList [ (n, t) | (BindName n, t) <- bts1]
   in   TermEnv (TermEnvTypes nts : bs2)


-- | Lookup a named type from an environment.
menvLookupType :: Name -> TermEnv a -> Maybe (Type a)
menvLookupType n (TermEnv bs0)
 = loop bs0
 where
        loop [] = Nothing
        loop (TermEnvValues{} : bs) = loop bs
        loop (TermEnvTypes nts : bs)
         = case Map.lookup n nts of
                Nothing         -> loop bs
                Just t          -> Just t


-- | Extend an environment with a new value.
menvExtendValue :: Bind -> Value a -> TermEnv a -> TermEnv a
menvExtendValue bb v env@(TermEnv evs)
 = case bb of
        BindName n -> TermEnv (TermEnvValues (Map.singleton n v) : evs)
        BindNone   -> env


-- | Extend an environment with some new values.
menvExtendValues :: [(Bind, Value a)] -> TermEnv a -> TermEnv a
menvExtendValues bvs1 (TermEnv bs2)
 = let  nvs = Map.fromList [ (n, v) | (BindName n, v) <- bvs1]
   in   TermEnv (TermEnvValues nvs : bs2)


