
module Salt.Core.Exp.Type.Base where
import Salt.Core.Exp.Name
import Data.Map                 (Map)
import qualified Data.Text      as T


-- | Annotated Type.
data Type a
        = TAnn a (Type a)                       -- ^ Annotated type.
        | TRef (TypeRef a)                      -- ^ Type reference.
        | TVar Bound                            -- ^ Type variable.
        | TAbs (TypeParams a) (Type a)          -- ^ Type abstraction.
        | TKey TypeKey [TypeArgs a]             -- ^ Type keyword application.
        deriving (Show, Eq, Ord)


-- | Kinds are represented the same way as types.
type Kind a = Type a


-- | Effects are also types, and we use this synonym to help readability.
type Effect a = Type a


-- | Type Reference.
data TypeRef a
        = TRPrm Name                            -- ^ Primitive type constructor.
        | TRCon Name                            -- ^ User defined type synonym or constructor.
        | TRClo (TypeClosure a)                 -- ^ Type closure.
        deriving (Show, Eq, Ord)


-- | Type Parameters.
data TypeParams a
        = TPAnn a (TypeParams a)
        | TPTypes [(Bind, Type a)]              -- ^ Type parameters.
        deriving (Show, Eq, Ord)


-- | Type Arguments.
data TypeArgs a
        = TGAnn a (TypeArgs a)
        | TGTypes [Type a]                      -- ^ Type arguments.
        deriving (Show, Eq, Ord)


-- | Type Key.
data TypeKey
        = TKHole                                -- ^ A missing type that needs to be inferred.
        | TKArr                                 -- ^ Kind arrow.
        | TKApp                                 -- ^ Type application.
        | TKFun                                 -- ^ Function type former.
        | TKForall                              -- ^ Forall type former.
        | TKExists                              -- ^ Exists type former.
        | TKRecord  [Name]                      -- ^ Record type former.
        | TKVariant [Name]                      -- ^ Variant type former.
        | TKSusp                                -- ^ Suspension type former.
        | TKSync                                -- ^ Top of the effect lattice.
        | TKPure                                -- ^ Bot of the effect lattice.
        | TKSum                                 -- ^ Effect sum.
        | TKReturn                              -- ^ Return type former.
        deriving (Show, Eq, Ord)


-- | Type Closure.
data TypeClosure a
        = TypeClosure (TypeEnv a) (TypeParams a) (Type a)
        deriving (Show, Eq, Ord)


-- | Environments captured in type closures.
data TypeEnv a
        = TypeEnv [TypeEnvBinds a]
        deriving (Show, Eq, Ord)


-- | Bindings in environments.
data TypeEnvBinds a
        = TypeEnvTypes (Map Name (Type a))
        deriving (Show, Eq, Ord)


instance IsString (Type a) where
 fromString name = TVar (Bound (Name $ T.pack name))
