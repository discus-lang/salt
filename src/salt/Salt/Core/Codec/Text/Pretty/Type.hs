
module Salt.Core.Codec.Text.Pretty.Type where
import Salt.Core.Codec.Text.Pretty.Base
import Salt.Core.Exp
import Salt.Data.Pretty
import qualified Data.Map       as Map


instance Pretty c (Type a) where
 ppr c tt
  = case tt of
        TAnn _ t -> ppr c t
        TRef r   -> ppr c r
        TVar u   -> ppr c u

        TAbs p t -> text "λ" %% ppr c p %% text "⇒" %% ppr c t

        THole    -> text "∙"

        TArr ksParam kResult
         -> squared (map (ppr c) ksParam)
         %% text "⇒" %% ppr c kResult

        TApp tFun tgsArg
         | tsArg <- takeTGTypes tgsArg
         -> pprTFun c tFun %% squared (map (pprTArg c) tsArg)

        TFun tsParam tsResult
         ->  squared (map (ppr c) tsParam)
         %%  text "→"
         %%  squared (map (ppr c) tsResult)

        TForall tps tBody
         ->  text "∀"
          %  squared  [ ppr c b % text ":" %% ppr c t
                      | (b, t) <- takeTPTypes tps ]
          %  text "." %% ppr c tBody

        TExists tps tBody
         ->  text "∃"
          %  squared  [ ppr c b % text ":" %% ppr c t
                      | (b, t) <- takeTPTypes tps ]
          %  text "." %% ppr c tBody

        TRecord ns tgss
         | length ns == length tgss
         -> text "∏" % squared
                [ pprLbl n % text ":"
                        %% (case takeTGTypes tgs of
                                [t] -> ppr c t
                                _   -> ppr c tgs)
                | n <- ns | tgs <- tgss ]

        TVariant ns tgss
         | length ns == length tgss
         -> text "∑" % squared
                [ pprLbl n % text ":"
                        %% (case takeTGTypes tgs of
                                [t] -> ppr c t
                                _   -> ppr c tgs)
                | n <- ns | tgs <- tgss ]

        TSusp tsv te
         -> squared (map (ppr c) tsv) % text "!" % pprTArg c te

        TSync     -> text "sync"
        TPure     -> text "pure"

        TSum ts
         -> let pp []              = text "pure"
                pp (t1 : [])       = ppr c t1
                pp (t1 : t2 : ts') = pprTArg c t1 %% text "+" %% pp (t2 : ts')
            in pp ts

        TKey k ts -> ppr c k %% (hsep $ map (ppr c) ts)


pprTFun c tt
 = case tt of
        TAnn _ t        -> pprTFun c t
        TRef{}          -> ppr c tt
        TVar{}          -> ppr c tt
        TApp{}          -> ppr c tt
        TRecord{}       -> ppr c tt
        TVariant{}      -> ppr c tt
        TSync{}         -> ppr c tt
        TPure{}         -> ppr c tt
        _               -> parens $ ppr c tt


pprTArg c tt
 = case tt of
        TAnn _ t        -> pprTArg c t
        TRef{}          -> ppr c tt
        TPrm{}          -> ppr c tt
        TVar{}          -> ppr c tt
        TRecord{}       -> ppr c tt
        TVariant{}      -> ppr c tt
        TSync{}         -> ppr c tt
        TPure{}         -> ppr c tt
        _               -> parens $ ppr c tt


instance Pretty c (TypeRef a) where
 ppr c tr
  = case tr of
        TRPrm n   -> pprPrm n
        TRCon n   -> pprCon n
        TRClo clo -> ppr c clo


instance Pretty c (TypeClosure a) where
 ppr c (TypeClosure (TypeEnv []) ps m)
  = bracketed' "tclo_"
        [ text "λ" % ppr c ps %% text "→" %% ppr c m ]

 ppr c (TypeClosure env ps m)
  = bracketed' "tclo"
        [ ppr c env
        , text "λ" % ppr c ps %% text "→" %% ppr c m ]


instance Pretty c (TypeEnv a) where
 ppr c (TypeEnv ebs)
  = bracketed' "tenv" (punctuate (text " ")
        $ map (ppr c) ebs)


instance Pretty c (TypeEnvBinds a) where
 ppr c eb
  = case eb of
        TypeEnvTypes  nts
         -> squared [ pprVar n %% text "=" %% ppr c t | (n, t) <- Map.toList nts ]


instance Pretty c (TypeArgs a) where
 ppr c tgs
  = case tgs of
        TGAnn _ tgs' -> ppr c tgs'
        TGTypes ts   -> squared $ map (ppr c) ts


instance Pretty c (TypeParams a) where
 ppr c tps
  = case tps of
        TPAnn _ tps' -> ppr c tps'
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
        TKReturn        -> text "##return"
