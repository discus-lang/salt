
module Salt.Core.Exp.TypeEnv where
import Salt.Core.Exp.Name
import Salt.Core.Exp.Type
import qualified Data.Map.Strict as Map


-- | Construct an empty term environment.
tenvEmpty :: TypeEnv a
tenvEmpty = TypeEnv []


-- | Extend a term environment with a new type.
tenvExtendType  :: Bind -> Type a -> TypeEnv a -> TypeEnv a
tenvExtendType bb t env@(TypeEnv evs)
 = case bb of
        BindName n      -> TypeEnv (TypeEnvTypes (Map.singleton n t) : evs)
        BindNone        -> env


-- | Extend an environment with some new types,
--   binding them all at the same level.
tenvExtendTypes :: [(Bind, Type a)] -> TypeEnv a -> TypeEnv a
tenvExtendTypes bts1 (TypeEnv bs2)
 = let  nts     = Map.fromList [ (n, t) | (BindName n, t) <- bts1]
   in   TypeEnv (TypeEnvTypes nts : bs2)


