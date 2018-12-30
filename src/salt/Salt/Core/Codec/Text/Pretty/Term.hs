
module Salt.Core.Codec.Text.Pretty.Term where
import Salt.Core.Codec.Text.Pretty.Type
import Salt.Core.Codec.Text.Pretty.Base
import Salt.Core.Exp
import Salt.Data.Pretty
import qualified Data.Set as Set
import qualified Data.Map as Map


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


instance Pretty c (TermClosure a) where
 ppr c (TermClosure (TermEnv []) ps m)
  = bracketed' "mclo_"
        [ text "λ" % ppr c ps %% text "→" %% ppr c m ]

 ppr c (TermClosure env ps m)
  = bracketed' "mclo"
        [ ppr c env
        , text "λ" % ppr c ps %% text "→" %% ppr c m ]


instance Pretty c (TermEnv a) where
 ppr c (TermEnv ebs)
  = bracketed' "menv" (punctuate (text " ")
        $ map (ppr c) ebs)


instance Pretty c (TermEnvBinds a) where
 ppr c eb
  = case eb of
        TermEnvTypes  nts
         -> text "@"
         %  squared [ pprVar n %% text "=" %% ppr c t | (n, t) <- Map.toList nts ]

        TermEnvValues nvs
         -> squared [ pprVar n %% text "=" %% ppr c v | (n, v) <- Map.toList nvs ]


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
                $ text "list" %% pprTArg c t % text "|"
                % hcat (punctuate (text ", ") (map (ppr c) vs))

        VSet  t vs
         -> brackets
                $ text "set"  %% pprTArg c t % text "|"
                % hcat (punctuate (text ", ") (map (ppr c) $ Set.toList vs))

        VMap  tk tv kvs
         -> brackets
                $ text "map"  %% pprTArg c tk %% pprTArg c tv % text "|"
                % hcat (punctuate (text ", ")
                               [ ppr c vk %% text ":=" %% ppr c vv
                               | (vk, vv) <- Map.toList kvs ])

        VClosure clo    -> ppr c clo

