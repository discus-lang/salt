
module Salt.Core.Codec.Text.Pretty.Term where
import Salt.Core.Codec.Text.Pretty.Type
import Salt.Core.Codec.Text.Pretty.Base
import Salt.Core.Exp
import Salt.Data.Pretty
import qualified Data.Set as Set
import qualified Data.Map as Map


------------------------------------------------------------------------------------------- Term --
instance Pretty c (Term a) where
 ppr c m = pprTerm c m

pprTerm :: c -> Term a -> Doc
pprTerm c (MAnn _ m)    = ppr c m
pprTerm c (MRef r)      = ppr c r
pprTerm c (MVar u)      = ppr c u

pprTerm c (MAbs p m)
 | isSomeMProc m
 = align $ text "λ" % ppr c p %% text "→" % line % ppr c m

 | otherwise
 = text "λ" % ppr c p %% text "→" %% ppr c m

pprTerm c (MRec bms mBody)
 =  text "rec" %% braced (map (ppr c) bms)
 %% text "in"  %% ppr c mBody

-- terms
pprTerm c (MTerms ms)
 = squared (map (ppr c) ms)

-- the
pprTerm c (MThe [t] m)
 = align $ text "the"   %% ppr c t  %% text "of" %% line % ppr c m

pprTerm c (MThe ts m)
 = align $ text "the"   %% squared (map (ppr c) ts )
                        %% text "of" %% line % ppr c m
-- app
pprTerm c (MAps mFun mgssArg)
 = pprMFun c mFun %% hcat (punctuate (text " ") $ map (pprMArgs c) mgssArg)

-- let
pprTerm c (MLet mps mBind mBody)
 | Just bts <- takeMPTerms mps
 = let pp (b, THole) = ppr c b
       pp (b, t)     = ppr c b % text ":" %% ppr c t
   in  case bts of
         [bt]   -> text "let" %% pp bt
                %% text "="   %% ppr c mBind
                %  text ";"   %% ppr c mBody

         _      -> text "let" %% squared (map pp bts)
                %% text "="   %% ppr c mBind
                %  text ";"   %% ppr c mBody

-- record / project
pprTerm c (MRecord ns ms)
 | length ns == length ms
 = text "∏" % squared [ pprLbl n %% text "=" %% ppr c m | n <- ns | m <- ms ]

pprTerm c (MProject l m)
 = pprMArg c m % text "." % pprLbl l

-- variant / varcase
pprTerm c (MVariant l m t)
 = text "the" %% ppr c t %% text "of" %% text "`" % pprLbl l %% pprMArg c m

pprTerm c (MVarCase mScrut msAlt [])
 = text "case" %% ppr c mScrut %% text "of"
         %% braced (map (pprMAlt c) msAlt)

pprTerm c (MVarCase mScrut msAlt [mElse])
 = text "case" %% ppr c mScrut %% text "of"
 %% braced (map (pprMAlt c) msAlt)
 %% text "else" %% ppr c mElse

-- data
pprTerm c (MData n ts ms)
 = pprCon n %% hsep (map (pprTArg c) ts) %% hsep (map (pprMArg c) ms)

-- box / run
pprTerm c (MBox m)
 = text "box" %% ppr c m

pprTerm c (MRun m)
 = text "run" %% ppr c m

-- list / set / map
pprTerm c (MList t ms)
 = brackets
        $ text "list" %% pprTArg c t % text "| "
        % hcat (punctuate (text ", ") (map (ppr c) ms))

pprTerm c (MSet  t ms)
 = brackets
        $ text "set"  %% pprTArg c t % text "| "
        % hcat (punctuate (text ", ") (map (ppr c) ms))

pprTerm c (MMap  tk tv msKey msVal)
 = brackets
        $ text "map"  %% pprTArg c tk %% pprTArg c tv % text "| "
        % hcat (punctuate (text ", ")
                [ ppr c mk %% text ":=" %% ppr c mv
                | mk <- msKey | mv <- msVal ])

-- proc launch / return
pprTerm c (MLaunch tsRet mRest)
 = align $  text "launch" %% squared (map (ppr c) tsRet) %% text "of"
         %% line % ppr c mRest

pprTerm c (MReturn mRet)
 = text "return" %% ppr c mRet

-- proc cell / update
pprTerm c (MCell nCell tCell mInit mRest)
 = align $ text "cell" %% pprVar nCell % text ":" %% ppr c tCell
                       %% text "←" %% ppr c mInit
 % semi %% line % ppr c mRest

pprTerm c (MUpdate nCell mValue mRest)
 = align $ text "update" %% pprVar nCell %% text "←" %% ppr c mValue
 % semi %% line % ppr c mRest

-- proc when / whens
pprTerm c (MWhens [mCond] [mThen] mRest)
 = align $ text "when" %% pprMArg c mCond %% ppr c mThen
 % semi %% line % ppr c mRest

-- proc loop / break / continue
pprTerm c (MLoop mBody mRest)
 = align $ text "loop" %% pprMArg c mBody
 % semi %% line % ppr c mRest

pprTerm _c MBreak    = text "break"
pprTerm _c MContinue = text "continue"

-- key
pprTerm c (MKey k mgs)
 = ppr c k %% (hsep $ map (ppr c) mgs)


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


pprMArgs :: c -> TermArgs a -> Doc
pprMArgs c mgs
 = case mgs of
        MGAnn _ mgs'    -> pprMArgs c mgs'
        MGTerm m        -> pprMArg c m

        MGTerms [m]     -> pprMArg c m
        MGTerms ms      -> squared $ map (pprTerm c) ms

        MGTypes [t]     -> text "@" % pprTArg c t
        MGTypes ts      -> text "@" % (squared $ map (pprTArg c) ts)


--------------------------------------------------------------------------------------- TermBind --
instance Pretty c (TermBind a) where
 ppr c (MBind b mpss tResult mBind)
  =  ppr c b
  %% hcat (map (ppr c) mpss)
  %  text ":" %% ppr c tResult
  %% text "=" %% ppr c mBind


---------------------------------------------------------------------------------------- TermRef --
instance Pretty c (TermRef a) where
 ppr c mr
  = case mr of
        MRVal v -> ppr c v
        MRPrm n -> pprPrm n
        MRCon n -> text "%" % pprCon n


------------------------------------------------------------------------------ TermParams / Args --
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


instance Pretty c (TermArgs a) where
 ppr c ma
  = case ma of
        MGAnn _ mgs -> ppr c mgs
        MGTerm m    -> parens $ ppr c m
        MGTerms ms  -> squared $ map (ppr c) ms
        MGTypes ts  -> text "@" % (squared $ map (ppr c) ts)


---------------------------------------------------------------------------------------- TermKey --
instance Pretty c TermKey where
 ppr _ mk
  = case mk of
        MKTerms         -> text "##terms"
        MKThe           -> text "##the"
        MKApp           -> text "##app"
        MKLet           -> text "##let"
        MKPrivate       -> text "##private"
        MKExtend        -> text "##extend"
        MKPack          -> text "##pack"
        MKUnpack        -> text "##unpack"
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

        MKSeq           -> text "##seq"
        MKLaunch        -> text "##launch"
        MKReturn        -> text "##return"
        MKCell          -> text "##cell"
        MKUpdate        -> text "##update"
        MKWhens         -> text "##whens"
        MKMatch         -> text "##match"
        MKLoop          -> text "##loop"
        MKBreak         -> text "##break"
        MKContinue      -> text "##continue"
        MKWhile         -> text "##while"
        MKEnter         -> text "##enter"
        MKLeave         -> text "##leave"


------------------------------------------------------------------------------------ TermClosure --
instance Pretty c (TermClosure a) where
 ppr c (TermClosure (TermEnv []) ps m)
  | isSomeMProc m
  = bracketed' "mclo_" [ align $ text "λ" % ppr c ps %% text "→" % line % ppr c m ]

  | otherwise
  = bracketed' "mclo_" [ text "λ" % ppr c ps %% text "→" %% ppr c m ]

 ppr c (TermClosure env ps m)
  | isSomeMProc m
  = bracketed' "mclo"
        [ ppr c env
        , align $ text "λ" % ppr c ps %% text "→" % line % ppr c m ]

  | otherwise
  = bracketed' "mclo"
        [ ppr c env
        , text "λ" % ppr c ps %% text "→" %% ppr c m ]


-------------------------------------------------------------------------------- TermEnv / Binds --
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


------------------------------------------------------------------------------------------ Value --
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
        VWord   i       -> string $ show i

        VInt8   i       -> string $ show i
        VInt16  i       -> string $ show i
        VInt32  i       -> string $ show i
        VInt64  i       -> string $ show i

        VWord8  i       -> string $ show i
        VWord16 i       -> string $ show i
        VWord32 i       -> string $ show i
        VWord64 i       -> string $ show i

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
        VBundle  bun    -> ppr c bun

        VLoc t i        -> brackets $ text "#loc" %% pprTArg c t %% int i

        VAddr a
         -> brackets
                $ text "address" % string (show a)

        VPtr r t a
         -> brackets
                $ text "pointer" %% pprTArg c r %% pprTArg c t % string (show a)

        -- {Value, AbstractedTypes, Ascription}
        VExtPair v t a
         -> brackets
                $ text "pack" %% pprVArg c v %% text "with" %% squared (map (ppr c) t) %% text "as" %% ppr c a

pprVArg c vv
 = case vv of
        VData{}         -> parens $ ppr c vv
        VVariant{}      -> parens $ ppr c vv
        _               -> ppr c vv


----------------------------------------------------------------------------------------- Bundle --
instance Pretty c (Bundle a) where
 ppr c (Bundle nts nms)
  = brackets
        $ align $ text "bundle|" % line
        % vcat  (punctuate (text ",")
                        (  (map (ppr c) $ Map.elems nts)
                        ++ (map (ppr c) $ Map.elems nms)))


instance Pretty c (BundleType a) where
 ppr c (BundleType _a n tpsParam kResult tBody)
  = vcat [ text "type" %% pprVar n
                %% hcat (map (ppr c) tpsParam)
                %  text ":" %% ppr c kResult
         , text " =  " % (align $ ppr c tBody) ]


instance Pretty c (BundleTerm a) where
 ppr c (BundleTerm _a n tmsParam tResult mBody)
  = vcat [ text "term" %% pprVar n
                %% hcat (map (ppr c) tmsParam)
                %  text ":" %% ppr c tResult
         , text " =  " % (align $ ppr c mBody) ]


-- | Pretty print the guts of a bundle, with declarations on sequential
--   lines, without the `[bundle| ]` wrapper.
ppBundleGuts ::  Bundle a -> Doc
ppBundleGuts (Bundle nts nms)
 = vcat (  (map (ppr ()) $ Map.elems nts)
        ++ (map (ppr ()) $ Map.elems nms))

