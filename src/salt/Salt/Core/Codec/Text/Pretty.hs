
module Salt.Core.Codec.Text.Pretty where
import Salt.Core.Exp
import Salt.Data.Pretty
import qualified Data.Set       as Set
import qualified Data.Map       as Map


---------------------------------------------------------------------------------------------------
instance Pretty c Bind where
 ppr c bb
  = case bb of
        BindName n      -> ppr c n
        BindNone        -> text "_"


instance Pretty c Bound where
 ppr c uu
  = case uu of
        Bound n         -> ppr c n


braced ds
 = braces    $ hcat $ punctuate (text "; ") ds

bracketed ds
 = brackets  $ hcat $ punctuate (text ", ") ds

bracketed' name ds
 = brackets  $ hcat (text name : text "|" : punctuate (text ", ") ds)

angled ds
 = text "⟨" % (hcat $ punctuate (text ", ") ds) % text "⟩"

squared ds
 = text "[" % (hcat $ punctuate (text ", ") ds) % text "]"


-- Type -------------------------------------------------------------------------------------------
instance Pretty c (Type a) where
 ppr c tt
  = case tt of
        TAnn _ t -> ppr c t
        TRef r   -> ppr c r
        TVar u   -> ppr c u

        TAbs p t
         -> text "λ" %% ppr c p %% text "⇒" %% ppr c t

        TKey TKHole []
         -> text "∙"

        -- Keyed expressions
        TKey TKArr   [TGTypes tsParam, TGTypes [tResult]]
         -> squared (map (ppr c) tsParam)
         %% text "⇒" %% ppr c tResult

        TKey TKApp   [TGTypes [tFun], TGTypes tsArg]
         -> pprTFun c tFun %% squared (map (pprTArg c) tsArg)

        TKey TKFun    [TGTypes tsParam, TGTypes tsResult]
         ->  squared (map (ppr c) tsParam)
         %%  text "→"
         %%  squared (map (ppr c) tsResult)

        TKey (TKRecord ns) [TGTypes ts]
         | length ns == length ts
         -> text "∏" % squared
                [ ppr c n % text ":" %% ppr c t
                | n <- ns | t <- ts ]

        TKey (TKVariant ns) [TGTypes ts]
         | length ns == length ts
         -> text "∑" % squared
                [ ppr c n % text ":" %% ppr c t
                | n <- ns | t <- ts ]

        TKey TKForall [TGTypes [TAbs (TPTypes bts) tBody]]
         ->  text "∀"
          %  squared  [ ppr c b % text ":" %% ppr c t
                      | (b, t) <- bts ]
          %  text "." %% ppr c tBody

        TKey TKExists [TGTypes [TAbs (TPTypes bts) tBody]]
         ->  text "∃"
          %  squared  [ ppr c b % text ":" %% ppr c t
                      | (b, t) <- bts ]
          %  text "." %% ppr c tBody

        TKey k ts
         ->  ppr c k %% (hsep $ map (ppr c) ts)


pprTFun c tt
 = case tt of
        TAnn _ t -> ppr c t
        TRef{}   -> ppr c tt
        TVar{}   -> ppr c tt
        -- Note: nested applications (TApp) are printed with parentheses to
        -- remove ambiguity between (f [x, y]) and ((f [x]) [y])
        _        -> parens $ ppr c tt


pprTArg c tt
 = case tt of
        TAnn _ t -> ppr c t
        TRef{}   -> ppr c tt
        TPrm{}   -> ppr c tt
        TVar{}   -> ppr c tt
        _        -> parens $ ppr c tt


instance Pretty c TypeRef where
 ppr c tr
  = case tr of
        TRPrm n -> text "#" % ppr c n
        TRCon n -> ppr c n


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
 ppr c tk
  = case tk of
        TKHole          -> text "##hole"
        TKArr           -> text "##arr"
        TKApp           -> text "##app"
        TKFun           -> text "##fun"
        TKForall        -> text "##forall"
        TKExists        -> text "##exists"
        TKRecord ns     -> text "##record"  %% bracketed (map (ppr c) ns)
        TKVariant ns    -> text "##variant" %% bracketed (map (ppr c) ns)


-- Term -------------------------------------------------------------------------------------------
instance Pretty c (Term a) where
 ppr c mm
  = case mm of
        MAnn _ m -> ppr c m
        MRef r   -> ppr c r

        MVar u
         -> ppr c u

        MAbs p m
         -> text "λ" %% ppr c p %% text "→" %% ppr c m

        MLet bts mBind mBody
         -> text "let" %% (squared [ppr c b % text ":" %% ppr c t | (b, t) <- bts])
         %% text "="   %% ppr c mBind
         %% text "in"  %% ppr c mBody

        MKey MKApp [MGTerms [mFun], MGTerms msArg]
         -> pprMFun c mFun %% squared (map (ppr c) msArg)

        MKey (MKProject n) [MGTerms [m]]
         -> ppr c m % text "." % ppr c n

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
        MRPrm n -> text "#" % ppr c n
        MRCon n -> text "%" % ppr c n

        MRTop ns n
         -> (hcat $ punctuate (text ".") (map (ppr c) ns))
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
 ppr c mk
  = case mk of
        MKTerms         -> text "##terms"
        MKThe           -> text "##the"
        MKApp           -> text "##app"
        MKLet           -> text "##let"
        MKCon n         -> text "##con"     %% ppr c n
        MKCase ns       -> text "##case"    %% braced (map (ppr c) ns)
        MKRecord ns     -> text "##record"  %% braced (map (ppr c) ns)
        MKProject n     -> text "##project" %% ppr c n
        MKVariant n     -> text "##variant" %% ppr c n
        MKIf            -> text "##if"
        MKList          -> text "##list"
        MKSet           -> text "##set"
        MKMap           -> text "##map"


-- Value ------------------------------------------------------------------------------------------
instance Pretty c (Value a) where
 ppr c
  = \case
        VUnit           -> text "#unit"
        VSymbol s       -> text "'" % ppr c s
        VText tx        -> string $ show tx

        VBool b
         -> case b of
                True    -> text "#true"
                False   -> text "#false"

        VInt  i         -> string $ show i
        VNat  i         -> string $ show i

        VData n ts vs   -> parens $ ppr c n
                                %% squared (map (ppr c) ts)
                                %% squared (map (ppr c) vs)

        VRecord nvs
         -> bracketed [ ppr c n %% text "=" %% ppr c v | (n, v) <- nvs ]

        VVariant n vs
         -> parens $  text "`" % ppr c n
                   %% squared (punctuate (text ", ") (map (ppr c) vs))

        VList t vs
         -> brackets
         $  hcat ( text "list" : ppr c t : text "|"
                 : punctuate (text ", ") (map (ppr c) vs))

        VSet  t vs
         -> brackets
         $  hcat ( text "set"  : ppr c t : text "|"
                 : punctuate (text ", ") (map (ppr c) $ Set.toList vs))

        VMap  tk tv kvs
         -> brackets
         $  hcat (text "map"   : ppr c tk : ppr c tv : text "|"
                 : punctuate (text ", ")
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
  = bracketed' "env"  (punctuate (text ",") $ map (ppr c) ebs)


instance Pretty c (EnvBind a) where
 ppr c eb
  = case eb of
        EnvType  n t    -> text "@" % ppr c n % text ":" %% ppr c t
        EnvValue n v    ->            ppr c n % text ":" %% ppr c v


instance Pretty c Name where
 ppr _c (Name n)
  = text n

