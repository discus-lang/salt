
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
        | VRecord   ![(Name, Value a)]          -- ^ Record value.
        | VList     !(Type a) ![Value a]        -- ^ List value.
        | VSet      !(Type a) !(Set (Value ())) -- ^ Set value.
        | VMap      !(Type a) !(Type a) !(Map (Value ()) (Value a))
                                                -- ^ Map value.
        | VClosure  !(Closure a)                -- ^ Closure.
        deriving (Show, Eq, Ord)


-- | Closure value.
data Closure a
        = CloTerm !(Env a) ![TermParams a] !(Term a)
        deriving (Show, Eq, Ord)


-- Patterns ---------------------------------------------------------------------------------------
pattern MVal v          = MRef  (MRVal v)
pattern MPrm n          = MRef  (MRPrm n)
pattern MCon n          = MRef  (MRCon n)

pattern MAbm bts mBody  = MAbs  (MPTerms bts) mBody
pattern MAbt bts mBody  = MAbs  (MPTypes bts) mBody

pattern MTerms ms       = MKey   MKTerms        [MGTerms ms]

pattern MThe ts m       = MKey   MKThe          [MGTypes ts, MGTerm m]

pattern MApp mFun mgs   = MKey   MKApp          [MGTerm  mFun, mgs]
pattern MApv mFun mArg  = MKey   MKApp          [MGTerm  mFun, MGTerm  mArg]
pattern MApm mFun msArg = MKey   MKApp          [MGTerm  mFun, MGTerms msArg]
pattern MApt mFun tsArg = MKey   MKApp          [MGTerm  mFun, MGTypes tsArg]

pattern MLet bts mb m   = MKey   MKLet          [MGTerms [mb, MAbs (MPTerms bts) m]]

pattern MIf mc mt me    = MKey   MKIf           [MGTerms mc,  MGTerms mt, MGTerm me]

pattern MRecord ns ms   = MKey  (MKRecord ns)   [MGTerms ms]
pattern MProject l m    = MKey  (MKProject l)   [MGTerms [m]]

pattern MVariant l m    = MKey  (MKVariant l)   [MGTerm   m]
pattern MCase m ls ms   = MKey  (MKCase ls)     [MGTerm  m,   MGTerms ms]

pattern MList t ms      = MKey   MKList         [MGTypes [t], MGTerms ms]
pattern MSet  t ms      = MKey   MKSet          [MGTypes [t], MGTerms ms]
pattern MMap  tk tv msk msv = MKey   MKMap      [MGTypes [tk, tv], MGTerms msk, MGTerms msv]

pattern MUnit           = MRef  (MRVal VUnit)
pattern MBool b         = MRef  (MRVal (VBool b))
pattern MTrue           = MBool  True
pattern MFalse          = MBool  False
pattern MInt i          = MRef  (MRVal (VInt i))
pattern MNat i          = MRef  (MRVal (VNat i))
pattern MSymbol n       = MRef  (MRVal (VSymbol n))
pattern MText tx        = MRef  (MRVal (VText tx))

pattern MSome t m       = MApm (MApt (MPrm (Name "Some")) [t]) [m]
pattern MNone t         = MApt (MPrm (Name "None")) [t]

-- Values
pattern VTrue           = VBool  True
pattern VFalse          = VBool  False
pattern VSome t v       = VData (Name "Some") [t] [v]
pattern VNone t         = VData (Name "None") [t] []
pattern VCloTerm e bs m = VClosure (CloTerm e bs m)


---------------------------------------------------------------------------------------------------
-- | Environments captured in closures.
data Env a
        = Env [(Name, (Value a))]
        deriving (Show, Eq, Ord)

-- | Construct an empty environment.
envEmpty :: Env a
envEmpty = Env []


-- | Extend an environment with a new value.
envExtend :: Bind -> Value a -> Env a -> Env a
envExtend (BindName n) v (Env bs)  = Env ((n, v) : bs)
envExtend BindNone _ env           = env


-- | Extend an environment with some new values.
envExtends :: [(Bind, Value a)] -> Env a -> Env a
envExtends bvs1 (Env nvs2) = Env ([(n, v) | (BindName n, v) <- bvs1] ++ nvs2)


-- | Lookup a named value from an environment.
envLookup :: Name -> Env a -> Maybe (Value a)
envLookup n (Env bs)       = lookup n bs


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

