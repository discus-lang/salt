
module Salt.Core.Check.WhereMsg where
import Salt.Core.Codec.Text.Pretty
import Salt.Core.Codec.Text         ()
import Salt.Core.Check.Where
import Salt.Data.Pretty


instance Show a => Pretty c (Where a) where
 ppr c wh = pprw c wh

-- type decls ---------------------------------------------
pprw _c (WhereTypeDecl _a n)
 = vcat [ text "In type declaration" %% pprNameQuoted n ]

-- term decls ---------------------------------------------
pprw _c (WhereTermDecl _a n)
 = vcat [ text "In term declaration" %% pprNameQuoted n ]

-- test decls ---------------------------------------------
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

-- terms --------------------------------------------------
pprw c  (WhereAppPrim _a n t)
 = vcat [ text "With " % squotes (pprPrm n) %% text "of type" %% ppr c t ]

pprw _c (WhereRecordField _a l Nothing)
 = vcat [ text "In field"  %% pprNameQuoted l ]

pprw c  (WhereRecordField _a l (Just t))
 = vcat [ text "In field"  %% pprNameQuoted l %% text "of type" %% ppr c t ]


