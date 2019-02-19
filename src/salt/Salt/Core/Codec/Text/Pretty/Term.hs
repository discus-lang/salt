
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
         -> text "λ" % ppr c p %% text "→" %% ppr c m

        MRec bms mBody
         ->  text "rec" %% braced (map (ppr c) bms)
          %% text "in"  %% ppr c mBody

        MTerms ms
         -> squared (map (ppr c) ms)

        MThe ts m
         -> case ts of
                [t] -> text "the" %% ppr c t  %% text "of" %% ppr c m
                _   -> text "the" %% squared (map (ppr c) ts )
                                  %% text "of" %% ppr c m

        MApp mFun mgsArg
         | Just tsArg <- takeMGTypes mgsArg
         -> case tsArg of
                [t] -> pprMFun c mFun %% text "@" % pprTArg c t
                _   -> pprMFun c mFun %% text "@" % squared (map (pprTArg c) tsArg)

         | Just msArg <- takeMGTerms mgsArg
         -> pprMFun c mFun %% squared (map (pprMArg c) msArg)

         | Just mArg <- takeMGTerm mgsArg
         -> pprMFun c mFun %% pprMArg c mArg

        MLet mps mBind mBody
         | Just bts <- takeMPTerms mps
         -> let pp (b, THole) = ppr c b
                pp (b, t)     = ppr c b % text ":" %% ppr c t
            in  case bts of
                 [bt]   -> text "let" %% pp bt
                        %% text "="   %% ppr c mBind
                        %  text ";"   %% ppr c mBody

                 _      -> text "let" %% squared (map pp bts)
                        %% text "="   %% ppr c mBind
                        %  text ";"   %% ppr c mBody

        MRecord ns ms
         | length ns == length ms
         -> text "∏" % squared [ pprLbl n %% text "=" %% ppr c m | n <- ns | m <- ms ]

        MProject l m
         -> pprMArg c m % text "." % pprLbl l

        MVariant l m t
         -> text "the" %% ppr c t %% text "of" %% text "`" % pprLbl l %% pprMArg c m

        MVarCase mScrut msAlt []
         -> text "case" %% ppr c mScrut %% text "of"
         %% braced (map (pprMAlt c) msAlt)

        MVarCase mScrut msAlt [mElse]
         -> text "case" %% ppr c mScrut %% text "of"
         %% braced (map (pprMAlt c) msAlt)
         %% text "else" %% ppr c mElse

        MData n ts ms
         -> pprCon n %% hsep (map (pprTArg c) ts) %% hsep (map (pprMArg c) ms)

        MRun m
         -> text "run" %% ppr c m

        MBox m
         -> text "box" %% ppr c m

        MList t ms
         -> brackets
                $ text "list" %% pprTArg c t % text "| "
                % hcat (punctuate (text ", ") (map (ppr c) ms))

        MSet  t ms
         -> brackets
                $ text "set"  %% pprTArg c t % text "| "
                % hcat (punctuate (text ", ") (map (ppr c) ms))

        MMap  tk tv msKey msVal
         -> brackets
                $ text "map"  %% pprTArg c tk %% pprTArg c tv % text "| "
                % hcat (punctuate (text ", ")
                               [ ppr c mk %% text ":=" %% ppr c mv
                               | mk <- msKey | mv <- msVal ])
        MKey k mgs
         -> ppr c k %% (hsep $ map (ppr c) mgs)


pprMFun c mm
 = case mm of
        MAnn _ m        -> pprMFun c m
        MRef{}          -> ppr c mm
        MVar{}          -> ppr c mm
        MApp{}          -> ppr c mm
        MAbs{}          -> parens $ ppr c mm
        MRec{}          -> parens $ ppr c mm
        MTerms{}        -> ppr c mm
        MRecord{}       -> ppr c mm
        MProject{}      -> ppr c mm
        MList{}         -> ppr c mm
        MSet{}          -> ppr c mm
        MMap{}          -> ppr c mm
        MKey{}          -> parens $ ppr c mm


pprMArg c mm
 = case mm of
        MAnn _ m        -> pprMArg c m
        MRef{}          -> ppr c mm
        MVar{}          -> ppr c mm
        MAbs{}          -> parens $ ppr c mm
        MRec{}          -> parens $ ppr c mm
        MTerms{}        -> ppr c mm
        MRecord{}       -> ppr c mm
        MProject{}      -> ppr c mm
        MList{}         -> ppr c mm
        MSet{}          -> ppr c mm
        MMap{}          -> ppr c mm
        MKey{}          -> parens $ ppr c mm


pprMAlt c mm
 = case mm of
        MVarAlt n mps mBody
         | Just bts <- takeMPTerms mps
         -> pprLbl n
                %% squared [ppr c b % text ":" %% ppr c t | (b, t) <- bts]
                %% text "→"
                %% ppr c mBody

        _ -> parens (ppr c mm)


instance Pretty c (TermBind a) where
 ppr c (MBind b mpss tResult mBind)
  =  ppr c b
  %% hcat (map (ppr c) mpss)
  %  text ":" %% ppr c tResult
  %% text "=" %% ppr c mBind


instance Pretty c (TermRef a) where
 ppr c mr
  = case mr of
        MRVal v -> ppr c v
        MRPrm n -> pprPrm n
        MRCon n -> text "%" % pprCon n


instance Pretty c (TermArgs a) where
 ppr c ma
  = case ma of
        MGAnn _ mgs -> ppr c mgs
        MGTerm m    -> parens $ ppr c m
        MGTerms ms  -> squared $ map (ppr c) ms
        MGTypes ts  -> text "@" % (squared $ map (ppr c) ts)


instance Pretty c (TermParams a) where
 ppr c mp
  = case mp of
        MPAnn _ mps -> ppr c mps

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
        MKBox           -> text "##box"
        MKRun           -> text "##run"
        MKList          -> text "##list"
        MKSet           -> text "##set"
        MKMap           -> text "##map"

        MKProc          -> text "##proc"
        MKProcYield     -> text "##proc'yield"
        MKProcCall      -> text "##proc'call"
        MKProcSeq       -> text "##proc'seq"
        MKProcWith      -> text "##proc'with"
        MKProcLaunch    -> text "##proc'launch"
        MKProcReturn    -> text "##proc'return"
        MKProcCell      -> text "##proc'cell"
        MKProcUpdate    -> text "##proc'update"
        MKProcWhen      -> text "##proc'when"
        MKProcMatch     -> text "##proc'match"
        MKProcLoop      -> text "##proc'loop"
        MKProcBreak     -> text "##proc'break"
        MKProcContinue  -> text "##proc'continue"

        MKBloc          -> text "##bloc"


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

        TermEnvValuesRec ncs
         -> text "μ"
         %  squared [ pprVar n %% text "=" %% ppr c v | (n, v) <- Map.toList ncs ]


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

        VNat     i       -> string $ show i
        VInt     i       -> string $ show i
        VWord    i       -> string $ show i

        VInt8    i       -> string $ show i
        VInt16   i       -> string $ show i
        VInt32   i       -> string $ show i
        VInt64   i       -> string $ show i

        VWord8   i       -> string $ show i
        VWord16  i       -> string $ show i
        VWord32  i       -> string $ show i
        VWord64  i       -> string $ show i

        VData n ts vs
         -> pprCon n %% hsep (map (pprTArg c) ts) %% hsep (map (pprVArg c) vs)

        VRecord nvs
         -> bracketed
                [ pprLbl n %% text "=" %% ppr c v | (n, v) <- nvs ]

        VVariant n t vs
         -> text "the" %% ppr c t
         %% text "of"  %% text "`" % pprLbl n %% squared (map (ppr c) vs)

        VList t vs
         -> brackets
                $ text "list" %% pprTArg c t % text "| "
                % hcat (punctuate (text ", ") (map (ppr c) vs))

        VSet  t vs
         -> brackets
                $ text "set"  %% pprTArg c t % text "| "
                % hcat (punctuate (text ", ") (map (ppr c) $ Set.toList vs))

        VMap  tk tv kvs
         -> brackets
                $ text "map"  %% pprTArg c tk %% pprTArg c tv % text "| "
                % hcat (punctuate (text ", ")
                               [ ppr c vk %% text ":=" %% ppr c vv
                               | (vk, vv) <- Map.toList kvs ])

        VClosure clo    -> ppr c clo

pprVArg c vv
 = case vv of
        VData{}         -> parens $ ppr c vv
        VVariant{}      -> parens $ ppr c vv
        _               -> ppr c vv


