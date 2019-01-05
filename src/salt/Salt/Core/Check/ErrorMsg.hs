
module Salt.Core.Check.ErrorMsg where
import Salt.Core.Codec.Text.Pretty
import Salt.Core.Codec.Text         ()
import Salt.Core.Check.Error
import Salt.Data.Pretty


instance Show a => Pretty c (Error a) where
 ppr c err = ppre c err

-- Malformed AST ------------------------------------------
ppre _c (ErrorKindMalformed _a _wh _k)
 = vcat [ text "Malformed kind." ]

ppre _c (ErrorTypeMalformed _a _wh _t)
 = vcat [ text "Malformed type." ]

ppre _c (ErrorTermMalformed _a _wc _m)
 = vcat [ text "Malformed term." ]


-- Module level problems ----------------------------------
ppre _c (ErrorTypeDeclsRecursive _a _wh nDecl nas)
 = vcat [ text "Recursive type declaration" %% squotes (pprVar nDecl)
        , text " Involving" %% squared [pprVar n | (n, _) <- nas] ]

ppre _c (ErrorTypeDeclRebound _a _wh nDecl)
 = vcat [ text "Rebound type name" %% squotes (pprVar nDecl)]

ppre _c (ErrorTermDeclRebound _a _wh nDecl)
 = vcat [ text "Rebound term name" %% squotes (pprVar nDecl)]

ppre c  (ErrorTermDeclImpure  _a _wh nDecl tEffect)
 = vcat [ text "Impure term declaration" %% squotes (pprVar nDecl)
        , text " has effect"    %% squotes (ppr c tEffect) ]

ppre _c (ErrorTestDeclRebound _a _wh nDecl)
 = vcat [ text "Rebound test"   %% squotes (pprVar nDecl) ]

ppre c  (ErrorTestDeclImpure  _a _wh mnDecl tEffect)
 = vcat [ text "Impure test declaration" %% maybe empty (squotes . pprVar) mnDecl
        , text " has effect"    %% squotes (ppr c tEffect) ]

ppre c  (ErrorTestDeclNotSusp  _a _wh mnDecl tsBody)
 = vcat [ text "Test" %% maybe empty (squotes . pprVar) mnDecl
                      %% text "does not produce a suspension to execute."
        , text " Actual types"  %% squared (map (ppr c) tsBody) ]


-- Structural arity ---------------------------------------
ppre c (ErrorTermsWrongArity _a _wh ts ks)
 = let  reason = if length ts >= length ks then "Too many" else "Not enough"
   in   vcat [ text reason %% text "values"
             , text " of types" %% squared (map (ppr c) ts)
             , text " Expected" %% squared (map (ppr c) ks) ]

ppre c (ErrorTypesWrongArity _a _wh ts ks)
 = let  reason = if length ts >= length ks then "Too many" else "Not enough"
   in   vcat [ text reason %% text "types"
             , text " of kinds" %% squared (map (ppr c) ts) ]


-- Unknown vars and refs ----------------------------------
ppre _ (ErrorUnknownPrimitive _a _wh n)
 = vcat [ text "Unknown primitive"        %% squotes (pprPrm n)]

ppre _ (ErrorUnknownDataCtor _a _wh n)
 = vcat [ text "Unknown data constructor" %% squotes (pprCon n)]

ppre _ (ErrorUnknownTypeCtor _a _wh n)
 = vcat [ text "Unknown type constructor" %% squotes (pprCon n)]

ppre _ (ErrorUnknownTypePrim _a _wh n)
 = vcat [ text "Unknown type primitive"   %% squotes (pprPrm n)]

ppre _ (ErrorUnknownKindCtor _a _wh n)
 = vcat [ text "Unknown kind constructor" %% squotes (text "#" % pprCon n)]

ppre c (ErrorUnknownTypeBound _a _wh u)
 = vcat [ text "Unknown type name"        %% squotes (ppr c u)]

ppre c (ErrorUnknownTermBound _a _wh u)
 = vcat [ text "Unknown term name"        %% squotes (ppr c u)]


-- Abstraction problems -----------------------------------
-- type
ppre c (ErrorAbsTypeImpure _a _wh eActual)
 = vcat [ text "Impure type abstraction body"
        , text " causes effect" %% ppr c eActual ]

ppre _ (ErrorAbsTypeBindConflict _a _wh ns)
 = vcat [ text "Conflicting type binders"
                %% squotes (hsep $ punctuate (text ",") $ map pprVar ns) ]

-- term
ppre c (ErrorAbsTermImpure _a _wh eActual)
 = vcat [ text "Impure term abstraction body"
        , text " causes effect" %% ppr c eActual ]

ppre _ (ErrorAbsTermBindConflict _a _wh ns)
 = vcat [ text "Conflicting term binders" %% squared (map pprVar ns) ]

ppre c (ErrorAbsTermNoValueForForall _a _wh ps)
 = vcat [ text "Polymorphic term abstraction does not produce a value."
        , text " Parameters" %% hsep (map (ppr c) ps) ]

-- Unexpected types ---------------------------------------
ppre c (ErrorTypeMismatch _a _wh tExpected tActual)
 = vcat [ text "Actual type"    %% squotes (ppr c tActual)
        , text " does not match"
        , text " expected type" %% squotes (ppr c tExpected) ]


-- Application problems -----------------------------------
ppre c (ErrorUnsaturatedPrim _a _wh n t)
 = vcat [ text "Unsaturated primitive" %% squotes (text "#" % pprLbl n)
        , text " of type" %% squotes (ppr c t) ]

ppre c (ErrorUnsaturatedCtor _a _wh n t)
 = vcat [ text "Unsaturated data constructor" %% squotes (pprLbl n)
        , text " of type" %% squotes (ppr c t) ]

ppre c (ErrorAppNoArguments _a _wh tFun)
 = vcat [ text "No arguments for function application"
        , text " of type" %% squotes (ppr c tFun) ]

-- type/type
ppre c (ErrorAppTypeTypeCannot _a _wh tFun)
 = vcat [ text "Cannot apply type"
        , text " of kind" %% squotes (ppr c tFun) ]

ppre c (ErrorAppTypeTypeWrongArity _a _wh ksExpected ksActual)
 | length ksExpected > length ksActual
 = vcat [ text "Not enough type arguments in application."
        , text " Parameter kinds" %% squared (map (ppr c) ksExpected)
        , text " Argument  kinds" %% squared (map (ppr c) ksActual) ]

 | otherwise
 = vcat [ text "Too many type arguments in application."
        , text " Parameter kinds" %% squared (map (ppr c) ksExpected)
        , text " Argument  types" %% squared (map (ppr c) ksActual) ]

ppre c (ErrorAppTypeTypeWrongArityNum _a _wh tsParam nArg)
 = let  reason = if nArg >= length tsParam then "Too many" else "Not enough"
   in   vcat [ text reason %% text "arguments in type application."
             , text " Parameter types" %% squared (map (ppr c) tsParam) ]

-- term/type
ppre c (ErrorAppTermTypeCannot _a _wh tFun)
 = vcat [ text "Cannot instantiate non-polymorphic value"
        , text " of type " %% squotes (ppr c tFun) ]

ppre c (ErrorAppTermTypeWrongArity _a _wh btsParam tsArg)
 | length btsParam > length tsArg
 = vcat [ text "Not enough type arguments in application."
        , text " Parameter kinds"
                %% squared [ (ppr c b %% text ":" %% ppr c t)
                           | (b, t) <- btsParam]
        , text " Argument types"
                %% squared (map (ppr c) tsArg) ]

 | otherwise
 = vcat [ text "Too many type arguments in application."
        , text " Parameter kinds"
                %% squared [ (ppr c b %% text ":" %% ppr c t)
                           | (b, t) <- btsParam]
        , text " Argument types"
                %% squared (map (ppr c) tsArg) ]

-- term/term
ppre c (ErrorAppTermTermCannot _a _wh tFun)
 = vcat [ text "Cannot apply non-function"
        , text " of type" %% squotes (ppr c tFun) ]

ppre c (ErrorAppTermTermWrongArity _a _wh tsParam tsArg)
 = let  reason = if length tsArg >= length tsParam then "Too many" else "Not enough"
   in   vcat [ text reason %% text "arguments in function application."
             , text " Parameter types" %% squared (map (ppr c) tsParam)
             , text " Argument types " %% squared (map (ppr c) tsArg) ]

ppre c (ErrorAppTermTermWrongArityNum _a _wh tsParam nArg)
 = let  reason = if nArg >= length tsParam then "Too many" else "Not enough"
   in   vcat [ text reason %% text "arguments in function application."
             , text " Parameter types" %% squared (map (ppr c) tsParam) ]


-- Let bindings -------------------------------------------
ppre c (ErrorLetWrongArity _a _wh tsActual bsExpected)
 = vcat [ text "Wrong aity in let binding"
        , text " Binders" %% squared (map (ppr c) bsExpected)
        , text " Values " %% squared (map (ppr c) tsActual) ]


-- Record problems ----------------------------------------
ppre c (ErrorRecordProjectIsNot _a _wh t n)
 = vcat [ text "Cannot take field" %% pprLbl n  %% text "from non-record"
        , text " of type" %% squotes (ppr c t) %% text "."]

ppre c (ErrorRecordProjectNoField _a _wh t n)
 = vcat [ text "Record does not have field"     %% squotes (pprLbl n) %% text "."
        , text " Actual type " %% ppr c t ]

ppre _ (ErrorRecordTypeDuplicateFields _a _wh ns)
 = vcat [ text "Duplicate fields in record type"
        , text " " % squared (map pprLbl ns) ]

ppre _ (ErrorRecordDuplicateFields _a _wh ns)
 = vcat [ text "Duplicate fields in record"
        , text " " % squared (map pprLbl ns) ]


-- Variant problems ---------------------------------------
ppre c (ErrorVariantAnnotIsNot _a _wh t)
 = vcat [ text "Variant annotation does not have variant type"
        , text " " %% squotes (ppr c t) ]

ppre c (ErrorVariantAnnotAltMissing _a _wh t n)
 = vcat [ text "Variant annotation is missing specified alternative."
        , text " Type " %% squotes (ppr c t)
        , text " Lacks" %% squotes (pprLbl n) ]

ppre _ (ErrorVariantTypeDuplicateAlts _a _wh ns)
 = vcat [ text "Duplicate alternatives in variant type"
        , text " " % braced (map pprLbl ns) ]

ppre c (ErrorCaseScrutNotVariant _a _wh t)
 = vcat [ text "Scrutinee does not have variant type"
        , text " " % squotes (ppr c t) ]

ppre c (ErrorCaseAltNotInVariant _a _wh n t)
 = vcat [ text "Alternative" %% squotes (pprLbl n)
                %% text "is not in scrutinee type"
        , text " " % squotes (ppr c t) ]

ppre c (ErrorCaseAltPatMismatch _a _wh n tAlt tScrut)
 = vcat [ text "Pattern does not match scrutinee type in alternative" %% squotes (pprLbl n)
        , text " Pattern type"   %% squotes (ppr c tAlt)
        , text " Scrutinee type" %% squotes (ppr c tScrut) ]

ppre c (ErrorCaseAltPatWrongArity _a _wh _nAlt tsPat tsField)
 = let  reason = if length tsPat >= length tsField then "Too many" else "Not enough"
   in   vcat [ text reason %% text "binders in pattern"
             , text " with field types" %% squared (map (ppr c) tsField) ]

ppre _c (ErrorCaseAltPatBindConflict _a _wh _nAlt nsDup)
 = vcat [ text "Duplicate binders in pattern" %% squared (map pprVar nsDup) ]

ppre _c (ErrorCaseAltsOverlapping _a _wh ns)
 = vcat [ text "Overlapping alternatives" %% squared (map pprLbl ns) ]

ppre c (ErrorCaseAltsInexhaustive _a _wh ns tScrut)
 = vcat [ text "Case has missing alternatives" %% squared (map pprLbl ns)
        , text " Scrutinee type" %% squotes (ppr c tScrut) ]


-- Suspension problems ------------------------------------
ppre c (ErrorRunSuspensionIsNot _a _wh ts)
 = vcat [ text "Cannot run non-suspension of type"
        , text " " %% squared (map (ppr c) ts) ]

