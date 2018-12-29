
module Salt.Core.Codec.Text.Pretty where
import Salt.Core.Exp
import Salt.Data.Pretty
import qualified Data.Set       as Set
import qualified Data.Map       as Map
import qualified Data.Text      as Text

import qualified Salt.Core.Codec.Text.Lexer as Lexer


---------------------------------------------------------------------------------------------------
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

braced ds
 = braces    $ hcat $ punctuate (text "; ") ds

bracketed ds
 = brackets  $ hcat $ punctuate (text ", ") ds

bracketed' name ds
 = brackets  $ hcat (text name : text "|" : punctuate (text ", ") ds)

squared ds
 = text "[" % (hcat $ punctuate (text ", ") ds) % text "]"

pprBump ((n, d), b)     = pprVar n % text "^" % integer d % text ":" % integer b


-- Type -------------------------------------------------------------------------------------------
instance Pretty c (Type a) where
 ppr c tt
  = case tt of
        TAnn _ t -> ppr c t
        TRef r   -> ppr c r
        TVar u   -> ppr c u

        TAbs p t
         -> text "λ" %% ppr c p %% text "⇒" %% ppr c t


        -- Keyed expressions
        THole    -> text "∙"

        TArr tsParam tResult
         -> squared (map (ppr c) tsParam)
         %% text "⇒" %% ppr c tResult

        TApt tFun tsArg
         -> pprTFun c tFun %% squared (map (pprTArg c) tsArg)

        TFun tsParam tsResult
         ->  squared (map (ppr c) tsParam)
         %%  text "→"
         %%  squared (map (ppr c) tsResult)

        TForall bks tBody
         ->  text "∀"
          %  squared  [ ppr c b % text ":" %% ppr c t
                      | (b, t) <- bks ]
          %  text "." %% ppr c tBody

        TExists bks tBody
         ->  text "∃"
          %  squared  [ ppr c b % text ":" %% ppr c t
                      | (b, t) <- bks ]
          %  text "." %% ppr c tBody

        TRecord ns tgs
         | length ns == length tgs
         -> text "∏" % squared
                [ pprLbl n % text ":"
                        %% (case tg of
                                TGTypes [t]     -> ppr c t
                                _               -> ppr c tg)
                | n <- ns | tg <- tgs ]

        TVariant ns tgs
         | length ns == length tgs
         -> text "∑" % squared
                [ pprLbl n % text ":"
                        %% (case tg of
                                TGTypes [t]     -> ppr c t
                                _               -> ppr c tg)
                | n <- ns | tg <- tgs ]

        TSusp tsv te
         -> squared (map (ppr c) tsv) % text "!" % pprTArg c te

        TSync   -> text "sync"
        TPure   -> text "pure"

        TSum ts -> hcat $ punctuate (text " + ") $ map (pprTArg c) ts

        TKey k ts
         ->  ppr c k %% (hsep $ map (ppr c) ts)


pprTFun c tt
 = case tt of
        TAnn _ t -> ppr c t
        TRef{}   -> ppr c tt
        TVar{}   -> ppr c tt
        TApp{}   -> ppr c tt
        _        -> parens $ ppr c tt


pprTArg c tt
 = case tt of
        TAnn _ t -> ppr c t
        TRef{}   -> ppr c tt
        TPrm{}   -> ppr c tt
        TVar{}   -> ppr c tt
        _        -> parens $ ppr c tt


instance Pretty c TypeRef where
 ppr _ tr
  = case tr of
        TRPrm n -> pprPrm n
        TRCon n -> pprCon n


instance Pretty c (TypeArgs a) where
 ppr c tgs
  = case tgs of
        TGTypes ts -> squared $ map (ppr c) ts


instance Pretty c (TypeParams a) where
 ppr c tps
  = case tps of
        TPTypes nts
         -> squared [ ppr c n % text ":" %% ppr c t
                   | (n, t) <- nts ]


instance Pretty c TypeKey where
 ppr _ tk
  = case tk of
        TKHole          -> text "##hole"
        TKArr           -> text "##arr"
        TKApp           -> text "##app"
        TKFun           -> text "##fun"
        TKForall        -> text "##forall"
        TKExists        -> text "##exists"
        TKRecord ns     -> text "##record"  %% bracketed (map pprLbl ns)
        TKVariant ns    -> text "##variant" %% bracketed (map pprLbl ns)
        TKSusp          -> text "##susp"
        TKSync          -> text "##sync"
        TKPure          -> text "##pure"
        TKSum           -> text "##sum"


instance Pretty c Ups where
 ppr _ (Ups bs)
  =     braced (map pprBump bs)


-- Term -------------------------------------------------------------------------------------------
instance Pretty c (Term a) where
 ppr c mm
  = case mm of
        MAnn _ m -> ppr c m
        MRef r   -> ppr c r

        MVar u   -> ppr c u

        MAbs p m
         -> text "λ" %% ppr c p %% text "→" %% ppr c m

        MThe t m
         -> text "the" %% ppr c t %% text "of" %% ppr c m

        MLet bts mBind mBody
         -> text "let" %% (squared [ppr c b % text ":" %% ppr c t | (b, t) <- bts])
         %% text "="   %% ppr c mBind
         %% text "in"  %% ppr c mBody

        -- TODO: pretty printing for rest of the term forms.
        MKey MKApp [MGTerms [mFun], MGTerms msArg]
         -> pprMFun c mFun %% squared (map (ppr c) msArg)

        MKey (MKProject n) [MGTerms [m]]
         -> ppr c m % text "." % pprLbl n

        MKey k ms
         -> ppr c k %% (hsep $ map (ppr c) ms)


pprMFun c mm
 = case mm of
        MAnn _ m -> ppr c m
        MRef{}   -> ppr c mm
        MVar{}   -> ppr c mm
        MAbs{}   -> parens $ ppr c mm
        MApp{}   -> ppr c mm
        MKey{}   -> parens $ ppr c mm


instance Pretty c (TermRef a) where
 ppr c mr
  = case mr of
        MRVal v -> ppr c v
        MRPrm n -> pprPrm n
        MRCon n -> text "%" % pprCon n

        MRTop ns n
         -> (hcat $ punctuate (text ".") (map pprCon ns))
                  % text "." % ppr c n

instance Pretty c (TermArgs a) where
 ppr c ma
  = case ma of
        MGTerm m   -> parens $ ppr c m
        MGTerms ms -> squared $ map (ppr c) ms
        MGTypes ts -> text "@" % (squared $ map (ppr c) ts)


instance Pretty c (TermParams a) where
 ppr c mp
  = case mp of
        MPTerms nts
         -> squared [ ppr c n % text ":" %% ppr c t
                    | (n, t) <- nts]

        MPTypes nts
         -> text "@"
         %  squared [ ppr c n % text ":" %% ppr c t
                    | (n, t) <- nts]


instance Pretty c TermKey where
 ppr _ mk
  = case mk of
        MKTerms         -> text "##terms"
        MKThe           -> text "##the"
        MKApp           -> text "##app"
        MKLet           -> text "##let"
        MKCon n         -> text "##con"     %% pprCon n
        MKRecord ns     -> text "##record"  %% braced (map pprCon ns)
        MKProject n     -> text "##project" %% pprLbl n
        MKVariant n     -> text "##variant" %% pprCon n
        MKVarCase       -> text "##var'case"
        MKVarAlt n      -> text "##var'alt" %% pprLbl n
        MKIf            -> text "##if"
        MKList          -> text "##list"
        MKSet           -> text "##set"
        MKMap           -> text "##map"
        MKBox           -> text "##box"
        MKRun           -> text "##run"



-- Value ------------------------------------------------------------------------------------------
instance Pretty c (Value a) where
 ppr c
  = \case
        VUnit           -> text "#unit"
        VSymbol s       -> pprSym s
        VText tx        -> string $ show tx

        VBool b
         -> case b of
                True    -> text "#true"
                False   -> text "#false"

        VNat    i       -> string $ show i

        VInt    i       -> string $ show i
        VInt8   i       -> string $ show i
        VInt16  i       -> string $ show i
        VInt32  i       -> string $ show i
        VInt64  i       -> string $ show i

        VWord   i       -> string $ show i
        VWord8  i       -> string $ show i
        VWord16 i       -> string $ show i
        VWord32 i       -> string $ show i
        VWord64 i       -> string $ show i

        VData n ts vs
         -> parens $ pprCon n
                %% squared (map (ppr c) ts)
                %% squared (map (ppr c) vs)

        VRecord nvs
         -> bracketed
                [ pprLbl n %% text "=" %% ppr c v | (n, v) <- nvs ]

        VVariant n t vs
         -> text "the" %% ppr c t
         %% text "of"  %% text "`" % pprLbl n %% squared (map (ppr c) vs)

        VList t vs
         -> brackets
                $  text "list" %% pprTArg c t % text "|"
                %  hcat (punctuate (text ", ") (map (ppr c) vs))

        VSet  t vs
         -> brackets
                $  text "set"  %% pprTArg c t % text "|"
                %  hcat (punctuate (text ", ") (map (ppr c) $ Set.toList vs))

        VMap  tk tv kvs
         -> brackets
                $  text "map"  %% pprTArg c tk %% pprTArg c tv % text "|"
                %  hcat (punctuate (text ", ")
                               [ ppr c vk %% text ":=" %% ppr c vv
                               | (vk, vv) <- Map.toList kvs ])

        VClosure clo    -> ppr c clo


instance Pretty c (Closure a) where
 ppr c (Closure (Env []) ps m)
  = bracketed' "clo"
        [ text "λ" % ppr c ps %% text "→" %% ppr c m ]

 ppr c (Closure env ps m)
  = bracketed' "clo"
        [ ppr c env
        , text "λ" % ppr c ps %% text "→" %% ppr c m ]


instance Pretty c (Env a) where
 ppr c (Env ebs)
  = bracketed' "env"  (punctuate (text " ") $ map (ppr c) ebs)


instance Pretty c (EnvBinds a) where
 ppr c eb
  = case eb of
        EnvTypes  nts
         -> text "@"
         %  squared [ pprVar n %% text "=" %% ppr c t | (n, t) <- Map.toList nts ]

        EnvValues nvs
         -> squared [ pprVar n %% text "=" %% ppr c v | (n, v) <- Map.toList nvs ]


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
