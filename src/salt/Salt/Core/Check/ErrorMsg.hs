
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
--        , string (show k) ]

ppre _c (ErrorTypeMalformed _a _wh _t)
 = vcat [ text "Malformed type." ]
--        , string (show t) ]

ppre _c (ErrorTermMalformed _a _wc _m)
 = vcat [ text "Malformed term." ]


-- Module level problems ----------------------------------
ppre _c (ErrorTypeDeclsRecursive _a _wh nDecl nas)
 = vcat [ text "Type declaration" %% squotes (pprVar nDecl) %% text "is recursive"
        , text "  though:" %% braced [pprVar n | (n, _) <- nas] ]

ppre _c (ErrorTypeDeclRebound _a _wh nDecl)
 = vcat [ text "Type" %% squotes (pprVar nDecl) %% text "declared multiple times." ]

-- Structural arity ---------------------------------------
ppre c (ErrorTermsWrongArity _a _wh ts ks)
 = let  reason = if length ts >= length ks then "Too many" else "Not enough"
   in   vcat [ text reason %% text "values"
             , text "  of types:" %% squared (map (ppr c) ts)
             , text "  expected:" %% squared (map (ppr c) ks) ]

ppre c (ErrorTypesWrongArity _a _wh ts ks)
 = let  reason = if length ts >= length ks then "Too many" else "Not enough"
   in   vcat [ text reason %% text "types"
             , text "  of kinds:" %% squared (map (ppr c) ts) ]


-- Unknown vars and refs ----------------------------------
ppre _ (ErrorUnknownPrimitive _a _wh n)
 = vcat [ text "Unknown primitive" %% squotes (pprPrm n) % text "." ]

ppre _ (ErrorUnknownDataCtor _a _wh n)
 = vcat [ text "Unknown data constructor" %% squotes (pprCon n) % text "."]

ppre _ (ErrorUnknownTypeCtor _a _wh n)
 = vcat [ text "Unknown type constructor" %% squotes (pprCon n) % text "."]

ppre _ (ErrorUnknownTypePrim _a _wh n)
 = vcat [ text "Unknown type primitive"   %% squotes (pprPrm n) % text "."]

ppre _ (ErrorUnknownKindCtor _a _wh n)
 = vcat [ text "Unknown kind constructor" %% squotes (pprCon n) % text "."]

ppre c (ErrorUnknownTypeBound _a _wh u)
 = vcat [ text "Type variable" %% squotes (ppr c u) %% text "is not in scope." ]

ppre c (ErrorUnknownTermBound _a _wh u)
 = vcat [ text "Variable" %% squotes (ppr c u) %% text "is not in scope." ]


-- Abstraction problems -----------------------------------
-- type
ppre c (ErrorAbsTypeImpure _a _wh eActual)
 = vcat [ text "Impure type abstraction"
        , text " causes effect" %% ppr c eActual ]

ppre _ (ErrorAbsTypeBindConflict _a _wh ns)
 = vcat [ text "Duplicate type binders at same level"
        , text " with names" %% braced (map pprVar ns) ]

-- term
ppre c (ErrorAbsTermImpure _a _wh eActual)
 = vcat [ text "Impure term abstraction"
        , text " causes effect" %% ppr c eActual ]

ppre _ (ErrorAbsTermBindConflict _a _wh ns)
 = vcat [ text "Duplicate term binders at same level"
        , text " with names" %% braced (map pprVar ns) ]


-- Unexpected types ---------------------------------------
ppre c (ErrorTypeMismatch _a _wh tExpected tActual)
 = vcat [ text "Unexpected type:" %% ppr c tActual
        , text "      expecting:" %% ppr c tExpected ]


-- Application problems -----------------------------------
ppre c (ErrorUnsaturatedPrim _a _wh n t)
 = vcat [ text "Unsaturated primitive #" % pprLbl n
        , text "  of type:" %% ppr c t ]

ppre c (ErrorUnsaturatedCtor _a _wh n t)
 = vcat [ text "Unsaturated data constructor" %% pprLbl n
        , text "  of type:" %% ppr c t ]

ppre c (ErrorAppNoArguments _a _wh tFun)
 = vcat [ text "No arguments for function application"
        , text "  of type:" %% ppr c tFun ]

-- type/type
ppre c (ErrorAppTypeTypeCannot _a _wh tFun)
 = vcat [ text "Cannot apply type"
        , text "  of kind:" %% ppr c tFun ]

ppre c (ErrorAppTypeTypeWrongArity _a _wh ksExpected ksActual)
 | length ksExpected > length ksActual
 = vcat [ text "Not enough type arguments in application."
        , text " parameter kinds:" %% squared (map (ppr c) ksExpected)
        , text "  argument kinds:" %% squared (map (ppr c) ksActual) ]

 | otherwise
 = vcat [ text "Too many type arguments in application."
        , text " parameter kinds:" %% squared (map (ppr c) ksExpected)
        , text "  argument types:" %% squared (map (ppr c) ksActual) ]

ppre c (ErrorAppTypeTypeWrongArityNum _a _wh tsParam nArg)
 = let  reason = if nArg >= length tsParam then "Too many" else "Not enough"
   in   vcat [ text reason %% text "arguments in type application."
             , text " parameter types:" %% squared (map (ppr c) tsParam) ]

-- term/type
ppre c (ErrorAppTermTypeCannot _a _wh tFun)
 = vcat [ text "Cannot instantiate non-polymorphic value "
        , text "  of type:" %% ppr c tFun ]

ppre c (ErrorAppTermTypeWrongArity _a _wh btsParam tsArg)
 | length btsParam > length tsArg
 = vcat [ text "Not enough type arguments in application."
        , text " parameter kinds:"
                %% squared [ (ppr c b %% text ":" %% ppr c t)
                           | (b, t) <- btsParam]
        , text "  argument types:"
                %% squared (map (ppr c) tsArg) ]

 | otherwise
 = vcat [ text "Too many type arguments in application."
        , text " parameter kinds:"
                %% squared [ (ppr c b %% text ":" %% ppr c t)
                           | (b, t) <- btsParam]
        , text "  argument types:"
                %% squared (map (ppr c) tsArg) ]

-- term/term
ppre c (ErrorAppTermTermCannot _a _wh tFun)
 = vcat [ text "Cannot apply non-function "
        , text "  of type:" %% ppr c tFun ]

ppre c (ErrorAppTermTermWrongArity _a _wh tsParam tsArg)
 = let  reason = if length tsArg >= length tsParam then "Too many" else "Not enough"
   in   vcat [ text reason %% text "arguments in function application."
             , text " parameter types:" %% squared (map (ppr c) tsParam)
             , text "  argument types:" %% squared (map (ppr c) tsArg) ]

ppre c (ErrorAppTermTermWrongArityNum _a _wh tsParam nArg)
 = let  reason = if nArg >= length tsParam then "Too many" else "Not enough"
   in   vcat [ text reason %% text "arguments in function application."
             , text " parameter types:" %% squared (map (ppr c) tsParam) ]


-- Let bindings -------------------------------------------
ppre c (ErrorLetWrongArity _a _wh tsActual bsExpected)
 = vcat [ text "Wrong aity in let binding "
        , text "  binders:" %% squared (map (ppr c) bsExpected)
        , text "   values:" %% squared (map (ppr c) tsActual) ]


-- Record problems ----------------------------------------
ppre c (ErrorRecordProjectIsNot _a _wh t n)
 = vcat [ text "Cannot project field"       %% pprLbl n %% text "from non-record"
        , text "  of type:" %% ppr c t ]

ppre c (ErrorRecordProjectNoField _a _wh t n)
 = vcat [ text "Record does not have field" %% pprLbl n
        , text "  actual type:" %% ppr c t ]

ppre _ (ErrorRecordTypeDuplicateFields _a _wh ns)
 = vcat [ text "Duplicate fields in record type"
        , text "  fields:" %% braced (map pprLbl ns) ]

ppre _ (ErrorRecordDuplicateFields _a _wh ns)
 = vcat [ text "Duplicate fields in record"
        , text "  fields:" %% braced (map pprLbl ns) ]


-- Variant problems ---------------------------------------
ppre c (ErrorVariantAnnotIsNot _a _wh t)
 = vcat [ text "Variant annotation does not have variant type"
        , text "  type:" %% ppr c t ]

ppre c (ErrorVariantAnnotAltMissing _a _wh t n)
 = vcat [ text "Variant annotation is missing specified alternative"
        , text "   type:" %% ppr c t
        , text "  lacks:" %% pprLbl n ]

ppre _ (ErrorVariantTypeDuplicateAlts _a _wh ns)
 = vcat [ text "Duplicate alternatives in variant type"
        , text "  alternatives:" %% braced (map pprLbl ns) ]

ppre c (ErrorCaseScrutNotVariant _a _wh t)
 = vcat [ text "Scrutinee does not have variant type"
        , text "  type:" %% ppr c t ]

ppre c (ErrorCaseAltNotInVariant _a _wh n t)
 = vcat [ text "Alternative is not in scrutinee type"
        , text "   alt:" %% pprLbl n
        , text "  type:" %% ppr c t ]

ppre c (ErrorCaseAltPatMismatch _a _wh n tAlt tScrut)
 = vcat [ text "Pattern type does not match scrutinee type"
        , text "          in alt:" %% pprLbl n
        , text "    pattern type:" %% ppr c tAlt
        , text "  scrutinee type:" %% ppr c tScrut ]

ppre c (ErrorCaseAltPatWrongArity _a _wh _nAlt tsPat tsField)
 = let  reason = if length tsPat >= length tsField then "Too many" else "Not enough"
   in   vcat [ text reason %% text "binders in pattern"
             , text "  with field types:" %% squared (map (ppr c) tsField) ]

ppre _c (ErrorCaseAltPatBindConflict _a _wh _nAlt nsDup)
 = vcat [ text "Duplicate binders in pattern"
        , text "  with names:" %% braced (map pprVar nsDup) ]

ppre _c (ErrorCaseAltsOverlapping _a _wh ns)
 = vcat [ text "Overlapping alternatives" %% braced (map pprLbl ns) ]

ppre c (ErrorCaseAltsInexhaustive _a _wh ns tScrut)
 = vcat [ text "Inexhaustive alternatives"
        , text "    missing alts:" %% braced (map pprLbl ns)
        , text "  scrutinee type:" %% ppr c tScrut ]


-- Suspension problems ------------------------------------
ppre c (ErrorRunSuspensionIsNot _a _wh ts)
 = vcat [ text "Cannot run non-suspension"
        , text "  of types:" %% squared (map (ppr c) ts) ]

