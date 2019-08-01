
module Salt.Core.Exp.Term.Patterns where
import Salt.Core.Exp.Term.Base
import Salt.Core.Exp.Name

------------------------------------------------------------------------------------------- Term --
pattern MVal v                  = MRef  (MRVal v)
pattern MPrm n                  = MRef  (MRPrm n)
pattern MCon n                  = MRef  (MRCon n)

pattern MAbm btsParam mBody
 = MAbs (MPTerms btsParam) mBody

pattern MAbt btsParam mBody
 = MAbs (MPTypes btsParam) mBody

pattern MTerms ms
 = MKey MKTerms         [MGTerms ms]

pattern MThe ts m
 = MKey MKThe           [MGTypes ts, MGTerm m]

pattern MAps mFun mgssArg
 = MKey MKApp           (MGTerm  mFun : mgssArg)

pattern MApp mFun mgsArg
 = MKey MKApp           [MGTerm  mFun, mgsArg]

pattern MApv mFun mArg
 = MKey MKApp           [MGTerm  mFun, MGTerm  mArg]

pattern MApm mFun msArg
 = MKey MKApp           [MGTerm  mFun, MGTerms msArg]

pattern MApt mFun tsArg
 = MKey MKApp           [MGTerm  mFun, MGTypes tsArg]

-- TODO: these are in wrong order.
pattern MLet mps mBind mBod
 = MKey MKLet           [MGTerm mBod, MGTerm (MAbs mps mBind)]

pattern MPrivate bksR btsW mBody
 = MKey MKPrivate       [MGTerm (MAbs (MPTypes bksR) (MAbs (MPTerms btsW) mBody))]

pattern MExtend r1 bksR btsW mBody
 = MKey MKExtend        [MGTypes [r1], MGTerm (MAbs (MPTypes bksR) (MAbs (MPTerms btsW) mBody))]

pattern MIf msCond msThen mElse
 = MKey   MKIf          [MGTerms msCond, MGTerms msThen, MGTerm mElse]

pattern MPack term abstractedTypes ascription
 = MKey MKPack [MGTerms [term], MGTypes abstractedTypes, MGTypes [ascription]]

pattern MUnpack mPacked rTermBinding rTypeBindings mBody
 = MKey MKUnpack [MGTerm mPacked, MGTerm (MAbs (MPTerms [rTermBinding]) (MAbs (MPTypes rTypeBindings) mBody))]

pattern MRecord  ns ms
 = MKey (MKRecord ns)   [MGTerms ms]

pattern MProject l  m
 = MKey (MKProject l)   [MGTerm  m]

pattern MVariant l m tResult
 = MKey (MKVariant l)   [MGTerm  m, MGTypes [tResult]]

pattern MVarCase mScrut msAlt msElse
 = MKey MKVarCase       [MGTerm  mScrut, MGTerms msAlt, MGTerms msElse]

pattern MVarAlt  n mps mBody
 = MKey (MKVarAlt n)    [MGTerm (MAbs mps mBody)]

pattern MData n ts ms
 = MKey  (MKCon n)      [MGTypes ts, MGTerms ms]

pattern MRun mBody
 = MKey   MKRun         [MGTerm mBody]

pattern MBox mBody
 = MKey   MKBox         [MGTerm mBody]

pattern MList tElem msElem
 = MKey   MKList        [MGTypes [tElem],  MGTerms msElem]

pattern MSet  tElem msElem
 = MKey   MKSet         [MGTypes [tElem],  MGTerms msElem]

pattern MMap  tk tv msKey msVal
 = MKey   MKMap         [MGTypes [tk, tv], MGTerms msKey, MGTerms msVal]


------------------------------------------------------------------------------------------- Proc --
pattern MSeq mps mBind mBody
 = MKey MKSeq           [MGTerm mBind, MGTerm (MAbs mps mBody)]

pattern MLaunch tsRet mRest
 = MKey MKLaunch        [MGTypes tsRet, MGTerm mRest]

pattern MReturn mRet
 = MKey MKReturn        [MGTerm mRet]

pattern MCell nCell tCell mInit mRest
 = MKey MKCell          [MGTerm mInit, MGTerm (MAbs (MPTerms [(BindName nCell, tCell)]) mRest)]

pattern MUpdate nCell mValue mRest
 = MKey MKUpdate        [MGTerm (MVar (Bound nCell)), MGTerm mValue, MGTerm mRest]

pattern MWhens msCond msThen mRest
 = MKey MKWhens         [MGTerms msCond, MGTerms msThen, MGTerm mRest]

pattern MMatch mScrut msAlt mRest
 = MKey MKMatch         [MGTerm mScrut, MGTerms msAlt, MGTerm mRest]

pattern MLoop mBody mRest
 = MKey MKLoop          [MGTerm mBody, MGTerm mRest]

pattern MBreak
 = MKey MKBreak         []

pattern MContinue
 = MKey MKContinue      []

pattern MWhile mPred mBody mRest
 = MKey MKWhile         [MGTerm mPred, MGTerm mBody, MGTerm mRest]

pattern MEnter mEnter bms mRest
 = MKey MKEnter         [MGTerm mEnter, MGTerm (MRec bms mRest)]

pattern MLeave
 = MKey MKLeave         []


------------------------------------------------------------------------------------------ Value --
pattern MUnit                   = MRef  (MRVal VUnit)
pattern MBool b                 = MRef  (MRVal (VBool b))
pattern MTrue                   = MRef  (MRVal (VBool True))
pattern MFalse                  = MRef  (MRVal (VBool False))
pattern MNat i                  = MRef  (MRVal (VNat i))
pattern MInt i                  = MRef  (MRVal (VInt i))
pattern MWord i                 = MRef  (MRVal (VWord i))
pattern MInt8 i                 = MRef  (MRVal (VInt8 i))
pattern MInt16 i                = MRef  (MRVal (VInt16 i))
pattern MInt32 i                = MRef  (MRVal (VInt32 i))
pattern MInt64 i                = MRef  (MRVal (VInt64 i))
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
