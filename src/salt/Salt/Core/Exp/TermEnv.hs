
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


-- | Extend an environemnt with some new values that recursively reference the new environment.
menvExtendValuesRec :: [(Bind, TermClosure a)] -> TermEnv a -> TermEnv a
menvExtendValuesRec bvs1 (TermEnv bs2)
 = let  ncs  = Map.fromList [ (n, clo) | (BindName n, clo) <- bvs1 ]
   in   TermEnv (TermEnvValuesRec ncs : bs2)


-- | Slice out the type portion of a `TermEnv` to produce a `TypeEnv`
menvSliceTypeEnv :: TermEnv a -> TypeEnv a
menvSliceTypeEnv (TermEnv evs)
 = TypeEnv $ goSlice evs
 where
        goSlice (TermEnvTypes nts : rest)
         = TypeEnvTypes nts : goSlice rest

        goSlice (TermEnvValues{}    : rest) = goSlice rest
        goSlice (TermEnvValuesRec{} : rest) = goSlice rest

        goSlice [] = []

