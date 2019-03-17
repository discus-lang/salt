
module Salt.Core.Codec.Text.Pretty.Base where
import Salt.Core.Exp
import Salt.Data.Pretty
import qualified Salt.Core.Codec.Text.Lexer as Lexer
import qualified Data.Text as Text


instance Pretty c Bind where
 ppr _ bb
  = case bb of
        BindName n      -> pprVar n
        BindNone        -> text "_"


instance Pretty c Bound where
 ppr _ uu
  = case uu of
        BoundWith n 0   -> pprVar n
        BoundWith n d   -> pprVar n % text "^" % integer d


instance Pretty c Universe where
 ppr _ vv
  = case vv of
        UKind           -> text "kind"
        UType           -> text "type"
        UTerm           -> text "term"


instance Pretty c Ups where
 ppr _ (Ups bs)
  =     braced (map pprBump bs)


instance Pretty c Fragment where
 ppr _ mm
  = case mm of
        FragTerm        -> text "term"
        FragProcBody    -> text "proc body"
        FragProcExp     -> text "proc exp"


braced ds
 = braces    $ hcat $ punctuate (text "; ") ds

bracketed ds
 = brackets  $ hcat $ punctuate (text ", ") ds

bracketed' name ds
 = brackets  $ hcat (text name : text "|" : punctuate (text ", ") ds)

squared ds
 = text "[" % (hcat $ punctuate (text ", ") ds) % text "]"

squoted ds
 = squotes (hsep $ punctuate (text ",") ds)

pprBump ((n, d), b)
 = pprVar n % text "^" % integer d % text ":" % integer b


pprNameAsIdentifier :: (Int -> Char -> Bool) -> Text -> Text -> Name -> Doc
pprNameAsIdentifier match ident_class prefix (Name name)
 | Text.length name > 0 && Lexer.checkMatch match (prefix <> name)
 = text (prefix <> name)
 | otherwise
 = text ("##" <> ident_class) <> string (show name)


pprVar :: Name -> Doc
pprVar = pprNameAsIdentifier Lexer.matchVar "Var" ""


-- | Labels are currently treated the same as variables in the lexer
pprLbl :: Name -> Doc
pprLbl = pprVar


pprCon :: Name -> Doc
pprCon = pprNameAsIdentifier Lexer.matchCon "Con" ""


pprSym :: Name -> Doc
pprSym = pprNameAsIdentifier Lexer.matchSym "Sym" "'"


pprPrm :: Name -> Doc
pprPrm = pprNameAsIdentifier Lexer.matchPrm "Prm" "#"


pprNameQuoted :: Name -> Doc
pprNameQuoted (Name name)
 = string (show name)
