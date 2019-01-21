
module Salt.Core.Exp.Term.Patterns where
import Salt.Core.Exp.Term.Base
import Salt.Core.Exp.Name

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

pattern MLet mps mBind mBod     = MKey   MKLet  [MGTerms [mBod, MAbs mps mBind]]

pattern MIf  mCond mThen mElse  = MKey   MKIf   [MGTerms mCond,  MGTerms mThen, MGTerm mElse]

pattern MProc mBody             = MKey   MKProc [MGTerm mBody]
pattern MBloc mBody             = MKey   MKBloc [MGTerm mBody]

pattern MRecord  ns ms          = MKey  (MKRecord ns) [MGTerms ms]
pattern MProject l  m           = MKey  (MKProject l) [MGTerm  m]

pattern MVariant l m tResult    = MKey  (MKVariant l) [MGTerm  m,      MGTypes [tResult]]
pattern MVarCase mScrut msAlt   = MKey   MKVarCase    [MGTerm  mScrut, MGTerms msAlt]
pattern MVarAlt  n mps mBody    = MKey  (MKVarAlt n)  [MGTerm (MAbs mps mBody)]

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
