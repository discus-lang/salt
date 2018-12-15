
module Salt.Core.Check.Pretty where
import Salt.Core.Codec.Text.Pretty
import Salt.Core.Codec.Text         ()
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Data.Pretty


---------------------------------------------------------------------------------------------------
instance Show a => Pretty c (Where a) where
 ppr c wh = pprw c wh

pprw _c (WhereTestKind _a Nothing)
 = vcat [ text "In kind test" ]

pprw _c (WhereTestKind _a (Just n))
 = vcat [ text "In kind test" %% pprNameQuoted n ]

pprw _c (WhereTestType _a Nothing)
 = vcat [ text "In type test" ]

pprw _c (WhereTestType _a (Just n))
 = vcat [ text "In type test" %% pprNameQuoted n ]

pprw _c (WhereTestEval _a Nothing)
 = vcat [ text "In eval test" ]

pprw _c (WhereTestEval _a (Just n))
 = vcat [ text "In eval test" %% pprNameQuoted n ]

pprw _c (WhereTestExec _a Nothing)
 = vcat [ text "In exec test" ]

pprw _c (WhereTestExec _a (Just n))
 = vcat [ text "In exec test" %% pprNameQuoted n ]

pprw _c (WhereTestAssert _a Nothing)
 = vcat [ text "In assert test" ]

pprw _c (WhereTestAssert _a (Just n))
 = vcat [ text "In assert test" %% pprNameQuoted n ]

pprw _c (WhereTermDecl _a n)
 = vcat [ text "In term declaration" %% pprNameQuoted n ]

pprw c  (WhereAppPrim _a n t)
 = vcat [ text "With " % squotes (pprPrm n) %% text "of type" %% ppr c t ]

pprw _c (WhereRecordField _a l Nothing)
 = vcat [ text "In field"  %% pprNameQuoted l ]

pprw c  (WhereRecordField _a l (Just t))
 = vcat [ text "In field"  %% pprNameQuoted l %% text "of type" %% ppr c t ]


---------------------------------------------------------------------------------------------------
instance Show a => Pretty c (Error a) where
 ppr c err = ppre c err

-- Malformed AST ------------------------------------------
ppre _c (ErrorTypeMalformed _a _wh _t)
 = vcat [ text "Malformed type." ]
--        , string (show t) ]

ppre _c (ErrorKindMalformed _a _wh _k)
 = vcat [ text "Malformed kind." ]
--        , string (show k) ]

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


-- Let bindings -------------------------------------------
ppre c (ErrorLetWrongArity _a _wh tsActual bsExpected)
 = vcat [ text "Wrong aity in let binding "
        , text "  binders:" %% squared (map (ppr c) bsExpected)
        , text "   values:" %% squared (map (ppr c) tsActual) ]


-- Purity problems ----------------------------------------
ppre c (ErrorImpureTypeAbstraction _a _wh eActual)
 = vcat [ text "Impure type abstraction"
        , text " causes effect" %% ppr c eActual ]

ppre c (ErrorImpureTermAbstraction _a _wh eActual)
 = vcat [ text "Impure term abstraction"
        , text " causes effect" %% ppr c eActual ]


-- Unexpected types ---------------------------------------
ppre c (ErrorTypeMismatch _a _wh tExpected tActual)
 = vcat [ text "Unexpected type:" %% ppr c tActual
        , text "      expecting:" %% ppr c tExpected ]

-- type/type
ppre c (ErrorAppTypeTypeCannot _a _wh tFun)
 = vcat [ text "Cannot apply type "
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


-- Variant problems ---------------------------------------
ppre _ (ErrorVariantTypeDuplicateAlts _a _wh ns)
 = vcat [ text "Duplicate alternatives in variant type"
        , text "  alternatives:" %% braced (map pprLbl ns) ]


-- Suspension problems ------------------------------------
ppre c (ErrorRunSuspensionIsNot _a _wh ts)
 = vcat [ text "Cannot run non-suspension"
        , text "  of types:" %% squared (map (ppr c) ts) ]

