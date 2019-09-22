
module Salt.Core.Check.ErrorMsg where
import Salt.Core.Check.WhereMsg         ()
import Salt.Core.Check.Error
import Salt.Core.Codec.Text.Pretty
import Salt.Core.Codec.Text             ()
import Salt.Core.Exp


---------------------------------------------------------------------------------------------------
instance Show a => Pretty c (Error a) where
 ppr c err = ppre c err

-- Malformed AST ------------------------------------------
ppre c (ErrorTypeMalformed uni _a _wh _k)
 = vcat [ text "Malformed" %% ppr c uni % text "." ]

ppre _c (ErrorTermMalformed _a _wh _m)
 = vcat [ text "Malformed term."]

ppre  c (ErrorTermNotFragment _a _wh mode txWhat)
 = vcat [ text txWhat %% text "cannot appear in a" %% ppr c mode % text "."]

-- Module level problems ----------------------------------
ppre _c (ErrorTypeDeclsRecursive _a _wh nDecl nas)
 = vcat [ text "Recursive type declaration" %% squotes (pprVar nDecl)
        , text " Involving" %% squoted [pprVar n | (n, _) <- nas] ]

ppre _c (ErrorTypeDeclRebound _a _wh nDecl)
 = vcat [ text "Rebound type name" %% squotes (pprVar nDecl)]

ppre _c (ErrorTermDeclRebound _a _wh nDecl)
 = vcat [ text "Rebound term name" %% squotes (pprVar nDecl)]

ppre _c (ErrorTestDeclRebound _a _wh nDecl)
 = vcat [ text "Rebound test"   %% squotes (pprVar nDecl) ]

ppre c  (ErrorTermDeclImpure  _a _wh nDecl tEffect)
 = vcat [ text "Impure term declaration" %% squotes (pprVar nDecl)
        , text " has effect"    %% squotes (ppr c tEffect) ]

ppre _c (ErrorTermDeclEmpty   _a _wh nDecl)
 = vcat [ text "Empty term declaration" %% squotes (pprVar nDecl) ]

ppre _c (ErrorTermDeclProcNoParams _a _wh nDecl)
 = vcat [ text "Proc declaration" %% squotes (pprVar nDecl) %% text "has no parameters" ]

ppre c  (ErrorTestDeclImpure  _a _wh mnDecl tEffect)
 = vcat [ text "Impure test declaration" %% maybe empty (squotes . pprVar) mnDecl
        , text " has effect"    %% squotes (ppr c tEffect) ]


-- Unknown vars and refs ----------------------------------
ppre c (ErrorUnknownPrim uni _a _wh n)
 = vcat [ text "Unknown" %% ppr c uni %% text "primitive"   %% squotes (pprPrm n)]

ppre c (ErrorUnknownCtor uni _a _wh n)
 = vcat [ text "Unknown" %% ppr c uni %% text "constructor" %% squotes (pprCon n)]

ppre c (ErrorUnknownBound uni _a _wh u)
 = vcat [ text "Unknown" %% ppr c uni %% text "name"        %% squotes (ppr c u)]


-- Structural arity ---------------------------------------
ppre c (ErrorWrongArity uni _a _wh ts ks)
 = let  reason = if length ts >= length ks then "Too many" else "Not enough"
   in   vcat [ text reason %% ppr c uni % text "s."
             , text " Actual  " %% ppThings'  c uni ts
             , text " Expected" %% ppThings'  c uni ks ]

ppre c (ErrorWrongArityUp uni _a _wh ts ks)
 = let  reason = if length ts >= length ks then "Too many" else "Not enough"
   in   vcat [ text reason %% ppr c uni % text "s."
             , text " Actual  " %% ppThings'  c uni ts
             , text " Expected" %% ppThings'' c uni ks ]


-- Abstraction problems -----------------------------------
ppre c (ErrorAbsConflict uni _a _wh ns)
 = vcat [ text "Conflicting" %% ppr c uni %% text "binders for"
                %% squoted (map pprVar ns)  ]

ppre c (ErrorAbsImpure uni _a _wh eActual)
 = vcat [ text "Impure" %% ppr c uni %% text "abstraction body"
        , text " causes effect" %% squotes (ppr c eActual) ]

ppre c (ErrorAbsEmpty uni _a _wh)
 = vcat [ text "Empty" %% ppr c uni %% text "abstraction body"
        , text " does not produce a value." ]

ppre c (ErrorAbsTermNoValueForForall _a _wh ps)
 = vcat [ text "Polymorphic term abstraction does not produce a value."
        , text " Parameters" %% squared (map (ppr c) ps) ]


-- Unexpected types ---------------------------------------
ppre c (ErrorMismatch uni _a _wh tActual tExpected)
 = vcat [ text "Actual" %% ppr c uni %% squotes (ppr c tActual)
        , text " does not match"
        , text " expected" %% ppr c uni %% squotes (ppr c tExpected) ]


-- Application problems -----------------------------------
ppre c (ErrorUnsaturatedPrim _a _wh n t)
 = vcat [ text "Unsaturated term primitive" %% squotes (text "#" % pprLbl n)
        , text " of type" %% squotes (ppr c t) ]

ppre c (ErrorUnsaturatedCtor _a _wh n t)
 = vcat [ text "Unsaturated term constructor" %% squotes (pprCon n)
        , text " of type" %% squotes (ppr c t) ]

ppre c (ErrorAppNoArguments _a _wh tFun)
 = vcat [ text "No arguments for function application"
        , text " of type" %% squotes (ppr c tFun) ]

ppre c (ErrorAppNotFunction _a _wh tFun)
 = vcat [ text "Cannot apply non-function"
        , text " of type" %% squotes (ppr c tFun) ]

ppre c (ErrorAppVector _a _wh tsFun)
 = vcat [ text "Cannot apply vector result"
        , text " of types" %% squotes (ppr c tsFun) ]

-- type/type
ppre c (ErrorAppTypeTypeCannot _a _wh tFun)
 = vcat [ text "Cannot apply type"
        , text " of kind" %% squotes (ppr c tFun) ]

ppre c (ErrorAppTypeTypeWrongArity _a _wh ksExpected ksActual)
 | length ksExpected > length ksActual
 = vcat [ text "Not enough type arguments in application."
        , text " Parameter" %% ppThings c UKind ksExpected
        , text " Argument " %% ppThings c UKind ksActual ]

 | otherwise
 = vcat [ text "Too many type arguments in application."
        , text " Parameter" %% ppThings c UKind ksExpected
        , text " Argument " %% ppThings c UKind ksActual ]

ppre c (ErrorAppTypeTypeWrongArityNum _a _wh tsParam nArg)
 = let  reason = if nArg >= length tsParam then "Too many" else "Not enough"
   in   vcat [ text reason %% text "arguments in type application."
             , case tsParam of
                [t] -> text " Expected argument of"  %% ppThings c UKind [t]
                _   -> text " Expected arguments of" %% ppThings c UKind tsParam ]

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
        , text " Argument " %% ppThings c UType tsArg ]

 | otherwise
 = vcat [ text "Too many type arguments in application."
        , text " Parameter kinds"
                %% squared [ (ppr c b %% text ":" %% ppr c t)
                           | (b, t) <- btsParam]
        , text " Argument " %% ppThings c UType tsArg ]

-- term/term
ppre c (ErrorAppTermTermCannot _a _wh tFun)
 = vcat [ text "Cannot apply non-function"
        , text " of type" %% squotes (ppr c tFun) ]

ppre c (ErrorAppTermTermWrongArity _a _wh tsParam tsArg)
 = let  reason = if length tsArg >= length tsParam then "Too many" else "Not enough"
   in   vcat [ text reason %% text "arguments in function application."
             , text " Parameter" %% ppThings c UType tsParam
             , text " Argument " %% ppThings c UType tsParam ]

ppre c (ErrorAppTermTermWrongArityNum _a _wh tsParam nArg)
 = let  reason = if nArg >= length tsParam then "Too many" else "Not enough"
   in   vcat [ text reason %% text "arguments in function application."
             , text " Parameter" %% ppThings c UType tsParam ]


-- Let bindings -------------------------------------------
ppre c (ErrorLetWrongArity _a _wh tsActual bsExpected)
 = vcat [ text "Wrong arity in let binding"
        , text " Binders" %% squared (map (ppr c) bsExpected)
        , text " Values " %% squared (map (ppr c) tsActual) ]


-- Rec bindings -------------------------------------------
ppre c (ErrorRecValueRecursion _a _wh b)
 = vcat [ text "Unsupported value recursion in binding" %% ppr c b ]

ppre _c (ErrorRecConflict _a _wh ns)
 = vcat [ text "Conflicting term" %% text "binders for"
                %% squoted (map pprVar ns)  ]


-- Record problems ----------------------------------------
ppre c (ErrorRecordProjectIsNot _a _wh t n)
 = vcat [ text "Cannot take field" %% pprLbl n %% text "from non-record"
        , text " of type" %% squotes (ppr c t) % text "."]

ppre c (ErrorRecordProjectNoField _a _wh t n)
 = vcat [ text "Record does not have field" %% squotes (pprLbl n) % text "."
        , text " Actual type " %% ppr c t ]

ppre _ (ErrorRecordTypeDuplicateFields _a _wh ns)
 = vcat [ text "Duplicate fields" %% squoted (map pprVar ns) %% text "in record type." ]

ppre _ (ErrorRecordDuplicateFields _a _wh ns)
 = vcat [ text "Duplicate fields" %% squoted (map pprVar ns) %% text "in record." ]


-- Variant problems ---------------------------------------
ppre c (ErrorVariantAnnotIsNot _a _wh t)
 = vcat [ text "Variant annotation does not have variant type."
        , text " Actual type" %% squotes (ppr c t) ]

ppre c (ErrorVariantAnnotAltMissing _a _wh t n)
 = vcat [ text "Variant annotation is missing specified alternative."
        , text " Type " %% squotes (ppr c t)
        , text " Lacks" %% squotes (pprLbl n) ]

ppre _ (ErrorVariantTypeDuplicateAlts _a _wh ns)
 = vcat [ text "Duplicate alternatives" %% squoted (map pprVar ns) %% text "in variant type." ]

ppre c (ErrorCaseScrutNotVariant _a _wh t)
 = vcat [ text "Scrutinee does not have variant type."
        , text " Actual type" %% squotes (ppr c t) ]

ppre c (ErrorCaseAltNotInVariant _a _wh n t)
 = vcat [ text "Alternative" %% squotes (pprLbl n)
                %% text "is not in scrutinee type."
        , text " " % squotes (ppr c t) ]

ppre c (ErrorCaseAltPatMismatch _a _wh n tAlt tScrut)
 = vcat [ text "Pattern does not match scrutinee type."
        , text " In alternative" %% squotes (pprLbl n)
        , text " Pattern type  " %% squotes (ppr c tAlt)
        , text " Scrutinee type" %% squotes (ppr c tScrut) ]

ppre c (ErrorCaseAltPatWrongArity _a _wh _nAlt tsPat tsField)
 = let  reason = if length tsPat >= length tsField then "Too many" else "Not enough"
   in   vcat [ text reason %% text "binders in pattern."
             , text " with field types" %% squared (map (ppr c) tsField) ]

ppre _c (ErrorCaseAltPatBindConflict _a _wh _nAlt nsDup)
 = vcat [ text "Duplicate binders in pattern" %% squoted (map pprVar nsDup) ]

ppre _c (ErrorCaseAltsOverlapping _a _wh ns)
 = vcat [ text "Overlapping alternatives" %% squoted (map pprLbl ns) ]

ppre c (ErrorCaseAltsInexhaustive _a _wh ns tScrut)
 = vcat [ text "Case has missing alternatives" %% squoted (map pprLbl ns)
        , text " Scrutinee type" %% squotes (ppr c tScrut) ]


-- Suspension problems ------------------------------------
ppre c (ErrorRunSuspensionIsNot _a _wh ts)
 = vcat [ text "Cannot run non-suspension"
        , text " of type" %% squared (map (ppr c) ts) ]


-- Procedure problems -------------------------------------
ppre _c (ErrorProcReturnNoLaunch _a _wh)
 = vcat [ text "return statement does not have an enclosing launch"]

ppre _c (ErrorProcBreakNoLoop _a _wh)
 = vcat [ text "break statement does not have an enclosing loop" ]

ppre _c (ErrorProcContinueNoLoop _a _wh)
 = vcat [ text "continue statement does not have an enclosing loop" ]

ppre c (ErrorProcUpdateNotCell _a _wh t)
 = vcat [ text "Value to update is not a cell"
        , text " of type" %% ppr c t ]


-- Existential problems -------------------------------------
ppre c (ErrorUnpackNotAppliedToPack _a _wh t)
 = vcat [ text "Value to unpack is not packed" %% squotes (ppr c t) ]

ppre c (ErrorPackTypeParamMismatch _a _wh aTypes ascription)
 = vcat [ text "Number of abstracted types" %% squared (map (ppr c) aTypes)
        , text " did not match number of existential type params" %% squotes (ppr c ascription) ]

ppre c (ErrorUnpackTypeParamMismatch _a _wh tBinds ascription)
 = vcat [ text "Number of type bindings" %% squared (map (ppr c) tBinds)
        , text " did not match number of existential type params" %% squotes (ppr c ascription) ]

ppre c (ErrorPackTypeNotExistential _a _wh t)
 = vcat [ text "Pack type ascription was not an existential" %% squotes (ppr c t) ]

---------------------------------------------------------------------------------------------------
-- | Print some universed things with proper pluralization.
ppThings  :: c -> Universe -> [Type a] -> Doc
ppThings c UKind [k]     = text "kind"    %% squotes (ppr c k)
ppThings c UKind ks      = text "kinds"   %% squared (map (ppr c) ks)
ppThings c UType [t]     = text "type"    %% squotes (ppr c t)
ppThings c UType ts      = text "types"   %% squared (map (ppr c) ts)
ppThings c UTerm [m]     = text "term"    %% squotes (ppr c m)
ppThings c UTerm ms      = text "terms"   %% squared (map (ppr c) ms)


-- | Print some universed things with proper pluralization,
--   taking the universe one level up.
ppThings' :: c -> Universe -> [Type a] -> Doc
ppThings' c UKind [k]     = text "thing"  %% squotes (ppr c k)
ppThings' c UKind ks      = text "things" %% squared (map (ppr c) ks)
ppThings' c UType [t]     = text "kind"   %% squotes (ppr c t)
ppThings' c UType ts      = text "kinds"  %% squared (map (ppr c) ts)
ppThings' c UTerm [m]     = text "type"   %% squotes (ppr c m)
ppThings' c UTerm ms      = text "types"  %% squared (map (ppr c) ms)


-- | Print some universed things with proper pluralization,
--   taking the universe two levels up.
ppThings'' :: c -> Universe -> [Type a] -> Doc
ppThings'' c UKind [k]     = text "thing"  %% squotes (ppr c k)
ppThings'' c UKind ks      = text "things" %% squared (map (ppr c) ks)
ppThings'' c UType [t]     = text "thing"  %% squotes (ppr c t)
ppThings'' c UType ts      = text "things" %% squared (map (ppr c) ts)
ppThings'' c UTerm [m]     = text "kind"   %% squotes (ppr c m)
ppThings'' c UTerm ms      = text "kinds"  %% squared (map (ppr c) ms)

