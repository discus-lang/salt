
module Salt.Core.Exp.Term where
import Salt.Core.Exp.Type
import Salt.Core.Exp.Name
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Int       as Int
import qualified Data.Word      as Word


---------------------------------------------------------------------------------------------------
-- | Annotated Term.
data Term a
        = MAnn      !a !(Term a)                -- ^ An annotated term.
        | MRef      !(TermRef a)                -- ^ Term reference.
        | MVar      !Bound                      -- ^ Term variable.
        | MAbs      !(TermParams a) !(Term a)   -- ^ Term abstraction.
        | MKey      !TermKey ![TermArgs a]      -- ^ Term keyword application.
        deriving (Show, Eq, Ord)


-- | Term Reference.
data TermRef a
        = MRPrm     !Name                       -- ^ Primitive reference.
        | MRCon     !Name                       -- ^ Data constructor reference.
        | MRVal     !(Value a)                  -- ^ Value reference.
        deriving (Show, Eq, Ord)


-- | Term Parameters.
data TermParams a
        = MPTerms   ![(Bind, Type a)]           -- ^ Term parameters for a term.
        | MPTypes   ![(Bind, Type a)]           -- ^ Type parameters for a term.
        deriving (Show, Eq, Ord)


-- | Term Arguments.
data TermArgs a
        = MGTerm    !(Term a)                   -- ^ Argument producing a vector of terms.
        | MGTerms   ![Term a]                   -- ^ Arguments of individual terms.
        | MGTypes   ![Type a]                   -- ^ Arguments of individual types.
        deriving (Show, Eq, Ord)


-- | Term Keyword.
data TermKey
        = MKTerms                               -- ^ Term sequence former.
        | MKThe                                 -- ^ Type ascription.

        | MKApp                                 -- ^ Term application.
        | MKLet                                 -- ^ Let expression former.

        | MKCon     !Name                       -- ^ Data constructor.

        | MKBox                                 -- ^ Box up a computation.
        | MKRun                                 -- ^ Run a computation.

        | MKRecord  ![Name]                     -- ^ Record former.
        | MKProject !Name                       -- ^ Record field projection.

        | MKVariant !Name                       -- ^ Variant former.
        | MKVarCase                             -- ^ Variant case matching.
        | MKVarAlt  !Name                       -- ^ Variant case alternative.

        | MKIf                                  -- ^ If-then-else expression.

        | MKList                                -- ^ List constructor.
        | MKSet                                 -- ^ Set constructor.
        | MKMap                                 -- ^ Map constructor.
        deriving (Show, Eq, Ord)


-- | Term Value.
--
--   These are really "term normal forms" rather than "values" as type
--   expressions and the bodies of closures may include variable names
--   that refer to top-level things. These names need to be bumped when
--   carrying values under binders.
--
data Value a
        -- Values that are also literals in the source program.
        = VUnit                                 -- ^ Unit value.
        | VBool     !Bool                       -- ^ Boolean value.
        | VNat      !Integer                    -- ^ Natural value.

        | VInt      !Integer                    -- ^ Integer value.
        | VInt8     !Int.Int8                   -- ^ 8 bit Integer value.
        | VInt16    !Int.Int16                  -- ^ 16 bit Integer value.
        | VInt32    !Int.Int32                  -- ^ 32 bit Integer value.
        | VInt64    !Int.Int64                  -- ^ 64 bit Integer value.

        | VWord     !Word.Word                  -- ^ Word value.
        | VWord8    !Word.Word8                 -- ^ 8 bit Word value.
        | VWord16   !Word.Word16                -- ^ 16 bit Word value.
        | VWord32   !Word.Word32                -- ^ 32 bit Word value.
        | VWord64   !Word.Word64                -- ^ 64 bit Word value.

        | VText     !Text                       -- ^ Text value.
        | VSymbol   !Name                       -- ^ Symbol value.

        -- Values that are only used at runtime.
        --   At runtime they are introduced by evaluating constructions,
        --   and do not appear as literals in the source program.
        --   The annotation on map and set elements is forced to () so that
        --   the order of values in the collection does not depend on the
        --   annotation.
        | VData     !Name ![Type a] ![Value a]  -- ^ Constructed data.
        | VRecord   ![(Name, [Value a])]        -- ^ Record value.
        | VVariant  !Name !(Type a) ![Value a]  -- ^ Variant value.
        | VList     !(Type a) ![Value a]        -- ^ List value.
        | VSet      !(Type a) !(Set (Value ())) -- ^ Set value.
        | VMap      !(Type a) !(Type a) !(Map (Value ()) (Value a))
                                                -- ^ Map value.
        | VClosure  !(TermClosure a)            -- ^ Closure.
        deriving (Show, Eq, Ord)


-- | Closure value.
data TermClosure a
        = TermClosure !(TermEnv a) !(TermParams a) !(Term a)
        deriving (Show, Eq, Ord)


-- | Environments captured in term closures.
data TermEnv a
        = TermEnv [TermEnvBinds a]
        deriving (Show, Eq, Ord)


-- | Bindings in environments.
data TermEnvBinds a
        = TermEnvTypes  (Map Name (Type a))
        | TermEnvValues (Map Name (Value a))
        deriving (Show, Eq, Ord)


-- | Normal form arguments for a function application.
--   Argument can be closed types as well as values.
data TermNormals a
        = NTs ![Type a]
        | NVs ![Value a]
        deriving (Show, Eq, Ord)


-- Patterns ---------------------------------------------------------------------------------------
pattern MVal v                  = MRef  (MRVal v)
pattern MPrm n                  = MRef  (MRPrm n)
pattern MCon n                  = MRef  (MRCon n)

pattern MAbm btsParam mBody     = MAbs  (MPTerms btsParam) mBody
pattern MAbt btsParam mBody     = MAbs  (MPTypes btsParam) mBody

pattern MTerms ms               = MKey   MKTerms [MGTerms ms]

pattern MThe ts m               = MKey   MKThe  [MGTypes ts, MGTerm m]

pattern MAps mFun mgssArg       = MKey   MKApp  (MGTerm  mFun : mgssArg)
pattern MApp mFun mgsArg        = MKey   MKApp  [MGTerm  mFun, mgsArg]
pattern MApv mFun mArg          = MKey   MKApp  [MGTerm  mFun, MGTerm  mArg]
pattern MApm mFun msArg         = MKey   MKApp  [MGTerm  mFun, MGTerms msArg]
pattern MApt mFun tsArg         = MKey   MKApp  [MGTerm  mFun, MGTypes tsArg]

pattern MLet btsBind mBind mBod = MKey   MKLet  [MGTerms [mBod, MAbs (MPTerms btsBind) mBind]]

pattern MIf  mCond mThen mElse  = MKey   MKIf   [MGTerms mCond,  MGTerms mThen, MGTerm mElse]

pattern MRecord  ns ms          = MKey  (MKRecord ns) [MGTerms ms]
pattern MProject l  m           = MKey  (MKProject l) [MGTerm  m]

pattern MVariant l m tResult    = MKey  (MKVariant l) [MGTerm  m,      MGTypes [tResult]]
pattern MVarCase mScrut msAlt   = MKey   MKVarCase    [MGTerm  mScrut, MGTerms msAlt]
pattern MVarAlt  n bts mBody    = MKey  (MKVarAlt n)  [MGTerm (MAbs (MPTerms bts) mBody)]

pattern MData    n ts ms        = MKey  (MKCon n)     [MGTypes ts, MGTerms ms]

pattern MRun  mBody             = MKey   MKRun  [MGTerm mBody]
pattern MBox  mBody             = MKey   MKBox  [MGTerm mBody]

pattern MList tElem msElem      = MKey   MKList [MGTypes [tElem],  MGTerms msElem]
pattern MSet  tElem msElem      = MKey   MKSet  [MGTypes [tElem],  MGTerms msElem]
pattern MMap  tk tv msKey msVal = MKey   MKMap  [MGTypes [tk, tv], MGTerms msKey, MGTerms msVal]


pattern MUnit                   = MRef  (MRVal VUnit)
pattern MBool b                 = MRef  (MRVal (VBool b))
pattern MTrue                   = MRef  (MRVal (VBool True))
pattern MFalse                  = MRef  (MRVal (VBool False))
pattern MNat i                  = MRef  (MRVal (VNat i))
pattern MInt i                  = MRef  (MRVal (VInt i))
pattern MInt8 i                 = MRef  (MRVal (VInt8 i))
pattern MInt16 i                = MRef  (MRVal (VInt16 i))
pattern MInt32 i                = MRef  (MRVal (VInt32 i))
pattern MInt64 i                = MRef  (MRVal (VInt64 i))
pattern MWord i                 = MRef  (MRVal (VWord i))
pattern MWord8 i                = MRef  (MRVal (VWord8 i))
pattern MWord16 i               = MRef  (MRVal (VWord16 i))
pattern MWord32 i               = MRef  (MRVal (VWord32 i))
pattern MWord64 i               = MRef  (MRVal (VWord64 i))
pattern MSymbol n               = MRef  (MRVal (VSymbol n))
pattern MText tx                = MRef  (MRVal (VText tx))

pattern MSome t m               = MApm (MApt (MPrm (Name "Some")) [t]) [m]
pattern MNone t                 = MApt (MPrm (Name "None")) [t]

-- Values
pattern VTrue                   = VBool  True
pattern VFalse                  = VBool  False
pattern VSome t v               = VData (Name "Some") [t] [v]
pattern VNone t                 = VData (Name "None") [t] []


-- Predicates -------------------------------------------------------------------------------------
-- | Check if this the boolean true value.
isVTrue :: Value a -> Bool
isVTrue (VBool True)    = True
isVTrue _               = False


-- Compounds --------------------------------------------------------------------------------------
-- | Take the top-level annotation from a type, if there is one.
takeAnnotOfTerm :: Term a -> Maybe a
takeAnnotOfTerm tt
 = case tt of
        MAnn a _ -> Just a
        _        -> Nothing


-- | Take the name of a primitive from a term, if this is one.
--   We look through any annotations and type ascriptions.
takeMPrm :: Term a -> Maybe Name
takeMPrm mm
 = case mm of
        MAnn _ m        -> takeMPrm m
        MThe _ m        -> takeMPrm m
        MPrm n          -> Just n
        _               -> Nothing


-- | Take the initial sequence of MGTypes from a list of TermArgs
takeMGTypesPrefix :: [TermArgs a] -> [TermArgs a]
takeMGTypesPrefix mgs
 = case mgs of
        mg@MGTypes{} : mgs' -> mg : takeMGTypesPrefix mgs'
        _                   -> []


-- | Unpack a Just wrapped closure, if this is one.
takeVMaybeClosure :: Value a -> Maybe (Maybe (TermClosure a))
takeVMaybeClosure vv
 = case vv of
        VSome _ (VClosure c) -> Just $ Just  c
        VNone _         -> Just $ Nothing
        _               -> Nothing


-- | Take a closure from a value, if this is one.
takeVClosure :: Value a -> Maybe (TermClosure a)
takeVClosure (VClosure c) = Just c
takeVClosure _            = Nothing


-- | Take the parameter and result types from a function type,
--   if this is one.
takeTFun :: Type a -> Maybe ([Type a], [Type a])
takeTFun tt
 = case tt of
        TFun tsParam tsResult   -> Just (tsParam, tsResult)
        _                       -> Nothing

