
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


-- Type -------------------------------------------------------------------------------------------
instance Pretty c (Type a) where
 ppr c tt
  = case tt of
        TAnn _ t -> ppr c t
        TRef r   -> ppr c r

        TVar u
         -> ppr c u

        TAbs p t
         -> text "λ" %% ppr c p %% text "→" %% ppr c t

        TKey TKHole []
         -> text "∙"

        -- Keyed expressions
        TKey TKTypes [TGTypes ts]
         -> braced $ map (ppr c) ts

        TKey TKApp   [TGTypes [tFun], TGTypes tsArg]
         -> pprTFun c tFun %% braced (map (pprTArg c) tsArg)

        TKey (TKPrim n) [TGTypes []]
         -> ppr c n

        TKey (TKPrim n) [TGTypes tsArg]
         -> ppr c n %% (hsep $ map (pprTArg c) tsArg)

        TKey (TKRecord []) [TGTypes []]
         -> text "[record|]"

        TKey (TKRecord ns) [TGTypes ts]
         | length ns == length ts
         -> bracketed [ ppr c n % text ":" %% ppr c t
                      | n <- ns | t <- ts ]

        TKey TKForall [TGTypes [TAbs (TPTypes bts) tBody]]
         -> text "∀"
          % braced    [ ppr c b % text ":" %% ppr c t
                      | (b, t) <- bts ]
          %% text "→"
          %% ppr c tBody

        TKey TKFun    [TGTypes tsParam, TGTypes tsResult]
         -> braced (map (ppr c) tsParam)
         %% text "→"
         %% braced (map (ppr c) tsResult)

        TKey k ts
         -> ppr c k %% (hsep $ map (ppr c) ts)


pprTFun c tt
 = case tt of
        TAnn _ t -> ppr c t
        TRef{}   -> ppr c tt
        TApp{}   -> ppr c tt
        _        -> parens $ ppr c tt


pprTArg c tt
 = case tt of
        TAnn _ t        -> ppr c t
        TRef{}          -> ppr c tt
        TPrim _ []      -> ppr c tt
        _               -> parens $ ppr c tt



instance Pretty c TypeRef where
 ppr c tr
  = case tr of
        TRCon n -> ppr c n


instance Pretty c (TypeArgs a) where
 ppr c tgs
  = case tgs of
        TGTypes ts -> braced $ map (ppr c) ts


instance Pretty c (TypeParams a) where
 ppr c tps
  = case tps of
        TPTypes nts
         -> braced [ ppr c n % text ":" %% ppr c t
                   | (n, t) <- nts ]


instance Pretty c TypeKey where
 ppr c tk
  = case tk of
        TKHole          -> text "##hole"
        TKTypes         -> text "##types"
        TKArr           -> text "##arr"
        TKApp           -> text "##app"
        TKFun           -> text "##fun"
        TKPrim n        -> text "##prim" %% ppr c n
        TKForall        -> text "##forall"
        TKRecord ns     -> text "##record" %% bracketed (map (ppr c) ns)


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
         -> text "let" %% (braced [ppr c b % text ":" %% ppr c t | (b, t) <- bts])
         %% text "="   %% ppr c mBind
         %% text "in"  %% ppr c mBody

        MKey MKApp [MGTerms [mFun], MGTerms msArg]
         -> pprMFun c mFun %% braced (map (ppr c) msArg)

        MKey (MKPrim n) [MGTypes [], mgts@(MGTerms _)]
         -> text "#" % ppr c n %% ppr c mgts

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

        MRTop ns n
         -> (hcat $ punctuate (text ".") (map (ppr c) ns))
                  % text "." % ppr c n

instance Pretty c (TermArgs a) where
 ppr c ma
  = case ma of
        MGTerms ms
         -> braced $ map (ppr c) ms

        MGTypes ts
         -> text "@"
         % (braced $ map (ppr c) ts)


instance Pretty c (TermParams a) where
 ppr c mp
  = case mp of
        MPTerms nts
         -> braced [ ppr c n % text ":" %% ppr c t
                   | (n, t) <- nts]

        MPTypes nts
         -> text "@"
         %  braced [ ppr c n % text ":" %% ppr c t
                   | (n, t) <- nts]


instance Pretty c TermKey where
 ppr c mk
  = case mk of
        MKTerms         -> text "##terms"
        MKApp           -> text "##app"
        MKLet           -> text "##let"
        MKCon n         -> text "##con"     %% ppr c n
        MKPrim n        -> text "##prim"    %% ppr c n
        MKCase ns       -> text "##case"    %% (hsep $ map (ppr c) ns)
        MKRecord ns     -> text "##record"  %% (hsep $ map (ppr c) ns)
        MKProject n     -> text "##project" %% ppr c n
        MKList          -> text "##list"
        MKSet           -> text "##set"
        MKMap           -> text "##map"
        MKHasType       -> text "##hastype"


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

        VData n []      -> ppr c n
        VData n vs      -> parens $ ppr c n %% braced (map (ppr c) vs)

        VRecord nvs     -> bracketed [ ppr c n %% text "=" %% ppr c v | (n, v) <- nvs ]

        VList vs        -> bracketed $ map (ppr c) vs

        VSet  vs        -> bracketed' "set"
                                $ map (ppr c) $ Set.toList vs

        VMap  kvs       -> bracketed' "map"
                                [ ppr c vk %% text ":=" %% ppr c vv
                                | (vk, vv) <- Map.toList kvs]

        VClosure clo    -> ppr c clo


instance Pretty c (Closure a) where
 ppr c (CloTerm (Env []) bs m)
  = bracketed' "clo"
        [ text "λ" % (hsep $ map (ppr c) bs) %% text "→" %% ppr c m ]

 ppr c (CloTerm env bs m)
  = bracketed' "clo"
        [ ppr c env
        , text "λ" % (hsep $ map (ppr c) bs) %% text "→" %% ppr c m ]


instance Pretty c (Env a) where
 ppr c (Env nvs)
  = bracketed' "env"  [ ppr c n %% text "=" %% ppr c v | (n, v) <- nvs]


instance Pretty c Name where
 ppr _c (Name n)
  = text n


