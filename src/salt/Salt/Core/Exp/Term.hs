
module Salt.Core.Exp.Term where
import Salt.Core.Exp.Type
import Salt.Core.Exp.Name
import Data.Map                 (Map)
import Data.Set                 (Set)


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
        = MRVal     !(Value a)                  -- ^ Value reference.
        | MRPrm     !Name                       -- ^ Primitive reference.
        | MRCon     !Name                       -- ^ Data constructor reference.
        | MRTop     ![Name] !Bound              -- ^ Reference to a top-level binding in a module.
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
        | MKCase    ![Name]                     -- ^ Case branching.
        | MKRecord  ![Name]                     -- ^ Record former.
        | MKProject !Name                       -- ^ Record field projection.
        | MKVariant !Name                       -- ^ Variant former.
        | MKIf                                  -- ^ If-then-else expression.
        | MKList                                -- ^ List constructor.
        | MKSet                                 -- ^ Set constructor.
        | MKMap                                 -- ^ Map constructor.
        | MKBox                                 -- ^ Box up a computation.
        | MKRun                                 -- ^ Run a computation.
        deriving (Show, Eq, Ord)


-- | Term Value.
data Value a
        -- Values that are also literals in the source program.
        = VUnit                                 -- ^ Unit value.
        | VBool     !Bool                       -- ^ Boolean value.
        | VNat      !Integer                    -- ^ Natural value.
        | VInt      !Integer                    -- ^ Integer value.
        | VText     !Text                       -- ^ Text value.
        | VSymbol   !Name                       -- ^ Symbol value.

        -- Values that are only used at runtime.
        --  At runtime they are introduced by evaluating constructions,
        --  and do not appear as literals in the source program.
        --  The annotation on map and set elements is forced to () so that the order
        --  of values in the collection does not depend on the annotation.
        | VData     !Name ![Type a] ![Value a]  -- ^ Constructed data.
        | VRecord   ![(Name, [Value a])]        -- ^ Record value.
        | VVariant  !Name !(Type a) ![Value a]  -- ^ Variant value.
        | VList     !(Type a) ![Value a]        -- ^ List value.
        | VSet      !(Type a) !(Set (Value ())) -- ^ Set value.
        | VMap      !(Type a) !(Type a) !(Map (Value ()) (Value a))
                                                -- ^ Map value.
        | VClosure  !(Closure a)                -- ^ Closure.
        deriving (Show, Eq, Ord)


-- | Closure value.
data Closure a
        = Closure !(Env a) !(TermParams a) !(Term a)
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
pattern MProject l  m           = MKey  (MKProject l) [MGTerms [m]]

pattern MVariant l ms tResult   = MKey  (MKVariant l) [MGTerms ms,     MGTypes [tResult]]
pattern MCase mScrut ls msAlt   = MKey  (MKCase ls)   [MGTerm  mScrut, MGTerms msAlt]

pattern MList tElem msElem      = MKey   MKList [MGTypes [tElem],  MGTerms msElem]
pattern MSet  tElem msElem      = MKey   MKSet  [MGTypes [tElem],  MGTerms msElem]
pattern MMap  tk tv msKey msVal = MKey   MKMap  [MGTypes [tk, tv], MGTerms msKey, MGTerms msVal]

pattern MRun  mBody             = MKey   MKRun  [MGTerm mBody]
pattern MBox  mBody             = MKey   MKBox  [MGTerm mBody]

pattern MUnit                   = MRef  (MRVal VUnit)
pattern MBool b                 = MRef  (MRVal (VBool b))
pattern MTrue                   = MRef  (MRVal (VBool True))
pattern MFalse                  = MRef  (MRVal (VBool False))
pattern MInt i                  = MRef  (MRVal (VInt i))
pattern MNat i                  = MRef  (MRVal (VNat i))
pattern MSymbol n               = MRef  (MRVal (VSymbol n))
pattern MText tx                = MRef  (MRVal (VText tx))

pattern MSome t m               = MApm (MApt (MPrm (Name "Some")) [t]) [m]
pattern MNone t                 = MApt (MPrm (Name "None")) [t]

-- Values
pattern VTrue                   = VBool  True
pattern VFalse                  = VBool  False
pattern VSome t v               = VData (Name "Some") [t] [v]
pattern VNone t                 = VData (Name "None") [t] []


---------------------------------------------------------------------------------------------------
-- | Environments captured in closures.
data Env a
        = Env [EnvBind a]
        deriving (Show, Eq, Ord)

data EnvBind a
        = EnvType  Name (Type a)
        | EnvValue Name (Value a)
        deriving (Show, Eq, Ord)


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



---------------------------------------------------------------------------------------------------
-- | Check if this the boolean true value.
isVTrue :: Value a -> Bool
isVTrue (VBool True)    = True
isVTrue _               = False


---------------------------------------------------------------------------------------------------
-- | Unpack a Just wrapped closure, if this is one.
takeVMaybeClosure :: Value a -> Maybe (Maybe (Closure a))
takeVMaybeClosure vv
 = case vv of
        VSome _ (VClosure c) -> Just $ Just  c
        VNone _         -> Just $ Nothing
        _               -> Nothing


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


-- | Take a closure from a value, if this is one.
takeVClosure :: Value a -> Maybe (Closure a)
takeVClosure (VClosure c) = Just c
takeVClosure _            = Nothing



-- | Take the parameter and result types from a function type,
--   if this is one.
takeTFun :: Type a -> Maybe ([Type a], [Type a])
takeTFun tt
 = case tt of
        TFun tsParam tsResult   -> Just (tsParam, tsResult)
        _                       -> Nothing

