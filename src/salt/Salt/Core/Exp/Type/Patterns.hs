
module Salt.Core.Exp.Type.Patterns where
import Salt.Core.Exp.Type.Base

-- Type refs.
pattern TCon n          = TRef (TRCon n)
pattern TPrm n          = TRef (TRPrm n)

-- Type keywords.
pattern THole           = TKey TKHole         []
pattern TArr    ks1 k2  = TKey TKArr          [TGTypes ks1,  TGTypes [k2]]
pattern TApp    tF  gs2 = TKey TKApp          [TGTypes [tF], gs2]
pattern TApt    tF  ts2 = TKey TKApp          [TGTypes [tF], TGTypes ts2]
pattern TFun    ts1 ts2 = TKey TKFun          [TGTypes ts1,  TGTypes ts2]
pattern TForall tps t   = TKey TKForall       [TGTypes [TAbs tps t]]
pattern TExists tps t   = TKey TKExists       [TGTypes [TAbs tps t]]
pattern TRecord  ns tgs = TKey (TKRecord  ns) tgs
pattern TVariant ns tgs = TKey (TKVariant ns) tgs
pattern TSusp   tsv te  = TKey TKSusp         [TGTypes tsv, TGTypes [te]]
pattern TPure           = TKey TKPure         []
pattern TSync           = TKey TKSync         []
pattern TSum    ts      = TKey TKSum          [TGTypes ts]


pattern (:=>) ks1 k2    = TArr    ks1 k2
infixr 4 :=>

pattern (:->) ts1 ts2   = TFun    ts1 ts2
infixr 4 :->

pattern (:*>) bks t     = TForall (TPTypes bks) t
infixr 3 :*>


-- Primitive types.
pattern TType           = TPrm "Type"
pattern TRepr           = TPrm "Repr"
pattern TData           = TPrm "Data"
pattern TComp           = TPrm "Comp"
pattern TState          = TPrm "State"
pattern TRegion         = TPrm "Region"
pattern TEffect         = TPrm "Effect"
pattern TProp           = TPrm "Prop"
pattern TUnit           = TPrm "Unit"
pattern TBool           = TPrm "Bool"
pattern TNat            = TPrm "Nat"
pattern TInt            = TPrm "Int"
pattern TWord           = TPrm "Word"
pattern TInt8           = TPrm "Int8"
pattern TInt16          = TPrm "Int16"
pattern TInt32          = TPrm "Int32"
pattern TInt64          = TPrm "Int64"
pattern TWord8          = TPrm "Word8"
pattern TWord16         = TPrm "Word16"
pattern TWord32         = TPrm "Word32"
pattern TWord64         = TPrm "Word64"
pattern TText           = TPrm "Text"
pattern TSymbol         = TPrm "Symbol"
pattern TOption t       = TApt (TPrm "Option") [t]
pattern TList t         = TApt (TPrm "List")   [t]
pattern TSet t          = TApt (TPrm "Set")    [t]
pattern TMap tk tv      = TApt (TPrm "Map")    [tk, tv]
pattern TCell t         = TApt (TPrm "Cell")    [t]
pattern TConsole        = TPrm "Console"
pattern TMemory         = TPrm "Memory"
pattern TSleep          = TPrm "Sleep"
pattern TAlloc r        = TApt (TPrm "Alloc") [r]
pattern TRead r         = TApt (TPrm "Read")  [r]
pattern TWrite r        = TApt (TPrm "Write") [r]
pattern TAddr           = TPrm "Addr"
pattern TPtr r t        = TApt (TPrm "Ptr") [r, t]
pattern TBundle         = TPrm "Bundle"
