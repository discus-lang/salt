
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
pprw _c (WhereTestDecl _a Nothing)
 = vcat [ text "In test" ]

pprw _c (WhereTestDecl _a (Just n))
 = vcat [ text "In test" %% pprNameQuoted n ]

-- terms --------------------------------------------------
pprw c  (WhereAppPrim _a n t)
 = vcat [ text "With " % squotes (pprPrm n) %% text "of type" %% ppr c t ]

pprw _c (WhereRecordField _a l Nothing)
 = vcat [ text "In field"  %% pprNameQuoted l ]

pprw c  (WhereRecordField _a l (Just t))
 = vcat [ text "In field"  %% pprNameQuoted l %% text "of type" %% ppr c t ]


