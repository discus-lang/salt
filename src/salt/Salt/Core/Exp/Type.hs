
module Salt.Core.Exp.Type where
import Salt.Core.Exp.Name
import Data.Map                 (Map)
import qualified Data.Text      as T


---------------------------------------------------------------------------------------------------
-- | Annotated Type.
data Type a
        = TAnn !a !(Type a)              -- ^ Annotated type.
        | TRef !(TypeRef a)              -- ^ Type reference.
        | TVar !Bound                    -- ^ Type variable.
        | TAbs !(TypeParams a) !(Type a) -- ^ Type abstraction.
        | TKey !TypeKey ![TypeArgs a]    -- ^ Type keyword application.
        deriving (Show, Eq, Ord)


-- | Kinds are represented the same way as types.
type Kind a = Type a


-- | Effects are also types, and we use this synonym to help readability.
type Effect a = Type a


-- | Type Reference.
data TypeRef a
        = TRPrm !Name                   -- ^ Primitive type constructor.
        | TRCon !Name                   -- ^ User defined type synonym or constructor.
        | TRClo (TypeClosure a)         -- ^ Type closure.
        deriving (Show, Eq, Ord)


-- | Type Parameters.
data TypeParams a
        = TPTypes ![(Bind, Type a)]     -- ^ Type parameters.
        deriving (Show, Eq, Ord)


-- | Type Arguments.
data TypeArgs a
        = TGTypes ![Type a]             -- ^ Type arguments.
        deriving (Show, Eq, Ord)


-- | Type Key.
data TypeKey
        = TKHole                        -- ^ A missing type that needs to be inferred.
        | TKArr                         -- ^ Kind arrow.
        | TKApp                         -- ^ Type application.
        | TKFun                         -- ^ Function type former.
        | TKForall                      -- ^ Forall type former.
        | TKExists                      -- ^ Exists type former.
        | TKRecord  ![Name]             -- ^ Record type former.
        | TKVariant ![Name]             -- ^ Variant type former.
        | TKSusp                        -- ^ Suspension type former.
        | TKSync                        -- ^ Top of the effect lattice.
        | TKPure                        -- ^ Bot of the effect lattice.
        | TKSum                         -- ^ Effect sum.
        deriving (Show, Eq, Ord)


-- | Type Closure.
data TypeClosure a
        = TypeClosure !(TypeEnv a) !(TypeParams a) !(Type a)
        deriving (Show, Eq, Ord)


-- | Environments captured in type closures.
data TypeEnv a
        = TypeEnv [TypeEnvBinds a]
        deriving (Show, Eq, Ord)


-- | Bindings in environments.
data TypeEnvBinds a
        = TypeEnvTypes (Map Name (Type a))
        deriving (Show, Eq, Ord)


-- Patterns ---------------------------------------------------------------------------------------
-- Type refs.
pattern TCon n          = TRef (TRCon n)
pattern TPrm n          = TRef (TRPrm n)

-- Type keywords.
pattern THole           = TKey TKHole         []
pattern TArr    ks1 k2  = TKey TKArr          [TGTypes ks1,  TGTypes [k2]]
pattern TApp    tF  gs2 = TKey TKApp          [TGTypes [tF], gs2]
pattern TApt    tF  ts2 = TKey TKApp          [TGTypes [tF], TGTypes ts2]
pattern TFun    ts1 ts2 = TKey TKFun          [TGTypes ts1,  TGTypes ts2]
pattern TForall bks t   = TKey TKForall       [TGTypes [TAbs (TPTypes bks) t]]
pattern TExists bks t   = TKey TKExists       [TGTypes [TAbs (TPTypes bks) t]]
pattern TRecord  ns mgs = TKey (TKRecord  ns) mgs
pattern TVariant ns mgs = TKey (TKVariant ns) mgs
pattern TSusp   tsv te  = TKey TKSusp         [TGTypes tsv, TGTypes [te]]
pattern TPure           = TKey TKPure         []
pattern TSync           = TKey TKSync         []
pattern TSum    ts      = TKey TKSum          [TGTypes ts]
pattern (:=>) ks1 k2    = TArr    ks1 k2
pattern (:->) ts1 ts2   = TFun    ts1 ts2
pattern (:*>) tps t     = TForall tps t

-- Primitive types.
pattern TData           = TPrm "Data"
pattern TRegion         = TPrm "Region"
pattern TEffect         = TPrm "Effect"
pattern TUnit           = TPrm "Unit"
pattern TBool           = TPrm "Bool"
pattern TNat            = TPrm "Nat"
pattern TInt            = TPrm "Int"
pattern TInt8           = TPrm "Int8"
pattern TInt16          = TPrm "Int16"
pattern TInt32          = TPrm "Int32"
pattern TInt64          = TPrm "Int64"
pattern TWord8          = TPrm "Word8"
pattern TWord           = TPrm "Word"
pattern TWord16         = TPrm "Word16"
pattern TWord32         = TPrm "Word32"
pattern TWord64         = TPrm "Word64"
pattern TText           = TPrm "Text"
pattern TSymbol         = TPrm "Symbol"
pattern TOption t       = TApt (TPrm "Option") [t]
pattern TList t         = TApt (TPrm "List")   [t]
pattern TSet t          = TApt (TPrm "Set")    [t]
pattern TMap tk tv      = TApt (TPrm "Map")    [tk, tv]
pattern TConsole        = TPrm "Console"
pattern TSleep          = TPrm "Sleep"


-- Instances --------------------------------------------------------------------------------------
instance IsString (Type a) where
 fromString name = TVar (Bound (Name $ T.pack name))


-- Predicates -------------------------------------------------------------------------------------
-- | Check if this is the data kind,
isTData :: Type a -> Bool
isTData tt
 = case tt of
        TData   -> True
        _       -> False


-- | Check if this type is the pure effect.
isTPure :: Type a -> Bool
isTPure tt
 = case tt of
        TPure    -> True
        _        -> False


-- | Check if this type is a suspension.
isTSusp :: Type a -> Bool
isTSusp tt
 = case tt of
        TSusp _ _ -> True
        _         -> False


-- Compounds --------------------------------------------------------------------------------------
-- | Given some type parameters and a body type,
--   if we have any parameters then build a type abstraction,
--   otherwise return the body type with no parameters.
makeTAbsOfParams :: [TypeParams a] -> Type a -> Type a
makeTAbsOfParams tps tBody
 = case tps of
        []      -> tBody
        _       -> foldr TAbs tBody tps


-- | If this is a `forall` type then split off the parameters and body.
takeTForalls :: Type a -> Maybe ([[(Bind, Type a)]], Type a)
takeTForalls tt
 = start
 where start
        = case tt of
                TForall tps tBody   -> Just $ go [tps] tBody
                _                   -> Nothing

       go tpss tBody
        = case tBody of
                TForall tps' tBody' -> go (tps' : tpss) tBody'
                _                   -> (reverse tpss, tBody)

