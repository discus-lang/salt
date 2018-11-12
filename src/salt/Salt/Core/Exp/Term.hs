
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
        | MKApp                                 -- ^ Term application.
        | MKLet                                 -- ^ Let expression former.
        | MKCon     !Name                       -- ^ Data constructor.
        | MKCase    ![Name]                     -- ^ Case branching.
        | MKRecord  ![Name]                     -- ^ Record former.
        | MKProject !Name                       -- ^ Record field projection.
        | MKList                                -- ^ List constructor.
        | MKSet                                 -- ^ Set constructor.
        | MKMap                                 -- ^ Map constructor.
        | MKHasType                             -- ^ Type ascription.
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
        --  The annotation on map values is forced to () so that the order
        --  of values in the map does not depend on the annotation.
        | VData     !Name ![Value a]            -- ^ Constructed data.
        | VRecord   ![(Name, Value a)]          -- ^ Record value.
        | VList     ![Value a]                  -- ^ List value.
        | VSet      !(Set (Value ()))           -- ^ Set value.
        | VMap      !(Map (Value ()) (Value a)) -- ^ Map value.
        | VClosure  !(Closure a)                -- ^ Closure.
        deriving (Show, Eq, Ord)


-- | Closure value.
data Closure a
        = CloTerm !(Env a) ![TermParams a] !(Term a)
        deriving (Show, Eq, Ord)


-- Patterns ---------------------------------------------------------------------------------------
pattern MVal v          = MRef  (MRVal v)
pattern MPrm n          = MRef  (MRPrm n)

pattern MAbm bts mBody  = MAbs  (MPTerms bts) mBody
pattern MAbt bts mBody  = MAbs  (MPTypes bts) mBody

pattern MTerms ms       = MKey   MKTerms        [MGTerms ms]
pattern MApp mFun mgs   = MKey   MKApp          [MGTerm  mFun, mgs]
pattern MApv mFun mArg  = MKey   MKApp          [MGTerm  mFun, MGTerm  mArg]
pattern MApm mFun msArg = MKey   MKApp          [MGTerm  mFun, MGTerms msArg]
pattern MApt mFun tsArg = MKey   MKApp          [MGTerm  mFun, MGTypes tsArg]
pattern MLet bts mb m   = MKey   MKLet          [MGTerms [mb, MAbs (MPTerms bts) m]]
pattern MCon  n ts ms   = MKey  (MKCon n)       [MGTypes ts, MGTerms ms]
pattern MProject l m    = MKey  (MKProject l)   [MGTerms [m]]
pattern MRecord ns ms   = MKey  (MKRecord ns)   [MGTerms ms]
pattern MList ms        = MKey   MKList         [MGTerms ms]
pattern MSet  ms        = MKey   MKSet          [MGTerms ms]
pattern MMap  msk msv   = MKey   MKMap          [MGTerms msk, MGTerms msv]

pattern MUnit           = MRef  (MRVal VUnit)
pattern MBool b         = MRef  (MRVal (VBool b))
pattern MTrue           = MBool  True
pattern MFalse          = MBool  False
pattern MInt i          = MRef  (MRVal (VInt i))
pattern MNat i          = MRef  (MRVal (VNat i))
pattern MSymbol n       = MRef  (MRVal (VSymbol n))
pattern MText tx        = MRef  (MRVal (VText tx))

pattern MJust t m       = MCon  (Name "Just")    [t] [m]
pattern MNothing t      = MCon  (Name "Nothing") [t] []

-- Values
pattern VTrue           = VBool  True
pattern VFalse          = VBool  False
pattern VJust v         = VData (Name "Just")    [v]
pattern VNothing        = VData (Name "Nothing") []
pattern VCloTerm e bs m = VClosure (CloTerm e bs m)


-- Projections ------------------------------------------------------------------------------------
takeVMaybeClosure :: Value a -> Maybe (Maybe (Closure a))
takeVMaybeClosure vv
 = case vv of
        VJust (VClosure c) -> Just $ Just c
        VNothing           -> Just $ Nothing
        _                  -> Nothing


---------------------------------------------------------------------------------------------------
-- | Environment of evaluation.
data Env a
        = Env [(Name, (Value a))]
        deriving (Show, Eq, Ord)

envEmpty :: Env a
envEmpty = Env []

envExtend :: Bind -> Value a -> Env a -> Env a
envExtend (BindName n) v (Env bs)  = Env ((n, v) : bs)
envExtend BindNone _ env           = env

envExtends :: [(Bind, Value a)] -> Env a -> Env a
envExtends bvs1 (Env nvs2) = Env ([(n, v) | (BindName n, v) <- bvs1] ++ nvs2)

envLookup :: Name -> Env a -> Maybe (Value a)
envLookup n (Env bs)       = lookup n bs


---------------------------------------------------------------------------------------------------
-- | Check if this the boolean true value.
isVTrue :: Value a -> Bool
isVTrue (VBool True)    = True
isVTrue _               = False


takeVClosure :: Value a -> Maybe (Closure a)
takeVClosure (VClosure c) = Just c
takeVClosure _            = Nothing


takeTFun :: Type a -> Maybe ([Type a], [Type a])
takeTFun tt
 = case tt of
        TFun tsParam tsResult   -> Just (tsParam, tsResult)
        _                       -> Nothing

