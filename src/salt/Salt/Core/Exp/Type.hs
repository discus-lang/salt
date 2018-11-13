
module Salt.Core.Exp.Type where
import Salt.Core.Exp.Name
import qualified Data.Text      as T


---------------------------------------------------------------------------------------------------
-- | Kinds are represented the same way as types.
type Kind a
        = Type a


-- | Annotated Type.
data Type a
        = TAnn !a !(Type a)              -- ^ Annotated type.
        | TRef !TypeRef                  -- ^ Type reference.
        | TVar !Bound                    -- ^ Type variable.
        | TAbs !(TypeParams a) !(Type a) -- ^ Type abstraction.
        | TKey !TypeKey ![TypeArgs a]    -- ^ Type keyword application.
        deriving (Show, Eq, Ord)


-- | Type Reference.
data TypeRef
        = TRPrm !Name                   -- ^ Primitive type constructor.
        | TRCon !Name                   -- ^ Used defined type constructor.
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
        deriving (Show, Eq, Ord)


-- Patterns ---------------------------------------------------------------------------------------
-- Type refs.
pattern TCon n          = TRef (TRCon n)
pattern TPrm n          = TRef (TRPrm n)

-- Type keywords.
pattern THole           = TKey TKHole        []
pattern TArr ks1 k2     = TKey TKArr         [TGTypes ks1,     TGTypes [k2]]
pattern TApp tFun mts2  = TKey TKApp         [TGTypes [tFun],  mts2]
pattern TApt tFun ts2   = TKey TKApp         [TGTypes [tFun],  TGTypes ts2]
pattern TFun ts1  ts2   = TKey TKFun         [TGTypes ts1,     TGTypes ts2]
pattern TForall  bks t  = TKey TKForall      [TGTypes [TAbs (TPTypes bks) t]]
pattern TExists  bks t  = TKey TKExists      [TGTypes [TAbs (TPTypes bks) t]]
pattern TRecord  ns  ts = TKey (TKRecord ns) [TGTypes ts]
pattern TVariant ns  ts = TKey (TKVariant ns) [TGTypes ts]
pattern (:=>) ks1 k2    = TArr    ks1 k2
pattern (:->) ts1 ts2   = TFun    ts1 ts2
pattern (:*>) tps t     = TForall tps t

-- Primitive types.
pattern TData           = TPrm "Data"
pattern TUnit           = TPrm "Unit"
pattern TBool           = TPrm "Bool"
pattern TNat            = TPrm "Nat"
pattern TInt            = TPrm "Int"
pattern TText           = TPrm "Text"
pattern TSymbol         = TPrm "Symbol"
pattern TOption t       = TApt (TPrm "Option") [t]
pattern TList t         = TApt (TPrm "List")   [t]
pattern TSet t          = TApt (TPrm "Set")    [t]
pattern TMap tk tv      = TApt (TPrm "Map")    [tk, tv]


-- Instances --------------------------------------------------------------------------------------
instance IsString (Type a) where
 fromString name = TVar (Bound (Name $ T.pack name))

