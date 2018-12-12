
module Salt.Core.Check.Pretty where
import Salt.Core.Codec.Text.Pretty
import Salt.Core.Codec.Text         ()
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Data.Pretty


---------------------------------------------------------------------------------------------------
instance Show a => Pretty c (Where a) where
 ppr c wh = pprw c wh

pprw _c (WhereTestType _a Nothing)
 = vcat [ text "In type test" ]

pprw c  (WhereTestType _a (Just n))
 = vcat [ text "In type test" %% squotes (ppr c n) ]

pprw _c (WhereTestEval _a Nothing)
 = vcat [ text "In eval test" ]

pprw c  (WhereTestEval _a (Just n))
 = vcat [ text "In eval test" %% squotes (ppr c n) ]

pprw _c (WhereTestExec _a Nothing)
 = vcat [ text "In exec test" ]

pprw c  (WhereTestExec _a (Just n))
 = vcat [ text "In exec test" %% squotes (ppr c n) ]

pprw _c (WhereTestAssert _a Nothing)
 = vcat [ text "In assert test" ]

pprw c  (WhereTestAssert _a (Just n))
 = vcat [ text "In assert test" %% squotes (ppr c n) ]

pprw c  (WhereTermDecl _a n)
 = vcat [ text "In term declaration" %% squotes (ppr c n) ]

pprw c  (WhereAppPrim _a n t)
 = vcat [ text "With #" % ppr c n %% text "of type" %% ppr c t ]

pprw c  (WhereRecordField _a l Nothing)
 = vcat [ text "In field"  %% squotes (ppr c l) ]

pprw c  (WhereRecordField _a l (Just t))
 = vcat [ text "In field"  %% squotes (ppr c l) %% text "of type" %% ppr c t ]


---------------------------------------------------------------------------------------------------
instance Show a => Pretty c (Error a) where
 ppr c err = ppre c err

-- Malformed AST ------------------------------------------
ppre _c (ErrorTypeMalformed _a t)
 = vcat [ text "Malformed type AST"
        , string (show t) ]

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
ppre c (ErrorUnknownPrimitive _a _wh n)
 = vcat [ text "Unknown primitive" %% squotes (text "#" % ppr c n) % text "." ]

ppre c (ErrorUnknownDataCtor _a _wh n)
 = vcat [ text "Unknown data constructor" %% squotes (ppr c n) % text "."]

ppre c (ErrorUnknownTypeBound _a _wh u)
 = vcat [ text "Variable" %% squotes (ppr c u) %% text "is not in scope." ]

ppre c (ErrorUnknownTermBound _a _wh u)
 = vcat [ text "Variable" %% squotes (ppr c u) %% text "is not in scope." ]


-- Let bindings -------------------------------------------
ppre c (ErrorLetWrongArity _a _wh tsActual bsExpected)
 = vcat [ text "Wrong aity in let binding "
        , text "  binders:" %% squared (map (ppr c) bsExpected)
        , text "   values:" %% squared (map (ppr c) tsActual) ]

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
                %% braced [ (ppr c b %% text ":" %% ppr c t)
                          | (b, t) <- btsParam]
        , text "  argument types:"
                %% braced (map (ppr c) tsArg) ]

 | otherwise
 = vcat [ text "Too many type arguments in application."
        , text " parameter kinds:"
                %% braced [ (ppr c b %% text ":" %% ppr c t)
                          | (b, t) <- btsParam]
        , text "  argument types:"
                %% braced (map (ppr c) tsArg) ]

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


-- Problems with records ----------------------------------
ppre c (ErrorRecordProjectIsNot _a _wh t n)
 = vcat [ text "Cannot project field"       %% squotes (ppr c n) %% text "from non-record"
        , text "  of type:" %% ppr c t ]

ppre c (ErrorRecordProjectNoField _a _wh t n)
 = vcat [ text "Record does not have field" %% squotes (ppr c n)
        , text "  actual type:" %% ppr c t ]

