
module Salt.Core.Transform.StripAnnot where
import Salt.Core.Exp.Module
import Salt.Core.Exp.Type
import Salt.Core.Exp.Term
import qualified Data.Map as Map


---------------------------------------------------------------------------------------------------
class StripAnnot c where
 stripAnnot :: c a -> c ()


---------------------------------------------------------------------------------------------------
instance StripAnnot Module where
 stripAnnot mm
        = Module
        { moduleDecls   = map stripAnnot $ moduleDecls mm}


---------------------------------------------------------------------------------------------------
instance StripAnnot Decl where
 stripAnnot dd
  = case dd of
        DType d         -> DType (stripAnnot d)
        DTerm d         -> DTerm (stripAnnot d)
        DTest d         -> DTest (stripAnnot d)
        DEmit d         -> DEmit (stripAnnot d)


instance StripAnnot DeclType where
 stripAnnot (DeclType _ n tps kResult tBody)
  = DeclType () n (map stripAnnot tps) (stripAnnot kResult) (stripAnnot tBody)


instance StripAnnot DeclTerm where
 stripAnnot (DeclTerm _ mode n mps tsResult mBody)
  = DeclTerm () mode n (map stripAnnot mps) (map stripAnnot tsResult) (stripAnnot mBody)


instance StripAnnot DeclTest where
 stripAnnot dd
  = case dd of
        DeclTestKind     _ w mn t   -> DeclTestKind     () w mn (stripAnnot t)
        DeclTestType     _ w mn m   -> DeclTestType     () w mn (stripAnnot m)
        DeclTestEvalType _ w mn t   -> DeclTestEvalType () w mn (stripAnnot t)
        DeclTestEvalTerm _ w mn m   -> DeclTestEvalTerm () w mn (stripAnnot m)
        DeclTestExec     _ w mn m   -> DeclTestExec     () w mn (stripAnnot m)
        DeclTestAssert   _ w mn m   -> DeclTestAssert   () w mn (stripAnnot m)


instance StripAnnot DeclEmit where
 stripAnnot dd
  = case dd of
        DeclEmit _ mn m -> DeclEmit () mn (stripAnnot m)


---------------------------------------------------------------------------------------------------
instance StripAnnot Type where
 stripAnnot tt
  = case tt of
        TAnn _ t        -> stripAnnot t
        TRef r          -> TRef (stripAnnot r)
        TVar u          -> TVar u
        TAbs p t        -> TAbs (stripAnnot p) (stripAnnot t)
        TKey k ts       -> TKey k (map stripAnnot ts)


instance StripAnnot TypeRef where
 stripAnnot tr
  = case tr of
        TRPrm n         -> TRPrm n
        TRCon n         -> TRCon n
        TRClo clo       -> TRClo (stripAnnot clo)


instance StripAnnot TypeParams where
 stripAnnot tps
  = case tps of
        TPAnn _ tps'    -> stripAnnot tps'
        TPTypes bts     -> TPTypes [(b, stripAnnot t) | (b, t) <- bts]


instance StripAnnot TypeArgs where
 stripAnnot tgs
  = case tgs of
        TGAnn _ tgs'    -> stripAnnot tgs'
        TGTypes ts      -> TGTypes (map stripAnnot ts)


instance StripAnnot TypeClosure where
 stripAnnot (TypeClosure env tps t)
  = TypeClosure (stripAnnot env) (stripAnnot tps) (stripAnnot t)


instance StripAnnot TypeEnv where
 stripAnnot (TypeEnv bs)
  = TypeEnv (map stripAnnot bs)


instance StripAnnot TypeEnvBinds where
 stripAnnot eb
  = case eb of
        TypeEnvTypes ts  -> TypeEnvTypes (Map.map stripAnnot ts)


---------------------------------------------------------------------------------------------------
instance StripAnnot Term where
 stripAnnot tt
  = case tt of
        MAnn _ m        -> stripAnnot m
        MRef r          -> MRef (stripAnnot r)
        MVar u          -> MVar u
        MAbs p m        -> MAbs (stripAnnot p) (stripAnnot m)
        MRec bms m      -> MRec (map stripAnnot bms) (stripAnnot m)
        MKey k ms       -> MKey k (map stripAnnot ms)


instance StripAnnot TermBind where
 stripAnnot (MBind b mpss ts m)
  = MBind b (map stripAnnot mpss) (map stripAnnot ts) (stripAnnot m)


instance StripAnnot TermRef where
 stripAnnot tr
  = case tr of
        MRVal v         -> MRVal (stripAnnot v)
        MRPrm n         -> MRCon n
        MRCon c         -> MRCon c


instance StripAnnot TermParams where
 stripAnnot mps
  = case mps of
        MPAnn _ m       -> stripAnnot m
        MPTerms bms     -> MPTerms [(b, stripAnnot m) | (b, m) <- bms]
        MPTypes bts     -> MPTypes [(b, stripAnnot t) | (b, t) <- bts]


instance StripAnnot TermArgs where
 stripAnnot mgs
  = case mgs of
        MGAnn _ mgs'    -> stripAnnot mgs'
        MGTerm  m       -> MGTerm  (stripAnnot m)
        MGTerms ms      -> MGTerms (map stripAnnot ms)
        MGTypes ts      -> MGTypes (map stripAnnot ts)


instance StripAnnot Value where
 stripAnnot vv
  = case vv of
        VUnit           -> VUnit
        VSymbol n       -> VSymbol n
        VText t         -> VText t
        VBool b         -> VBool b
        VNat n          -> VNat n
        VInt i          -> VInt i
        VWord i         -> VWord i
        VInt8 i         -> VInt8 i
        VInt16 i        -> VInt16 i
        VInt32 i        -> VInt32 i
        VInt64 i        -> VInt64 i
        VWord8 i        -> VWord8 i
        VWord16 i       -> VWord16 i
        VWord32 i       -> VWord32 i
        VWord64 i       -> VWord64 i
        VData n ts vs   -> VData n    (map stripAnnot ts) (map stripAnnot vs)
        VRecord  nvss   -> VRecord    [ (n, map stripAnnot vs) | (n, vs) <- nvss ]
        VVariant n t vs -> VVariant n (stripAnnot t)  (map stripAnnot vs)
        VList t vs      -> VList      (stripAnnot t)  (map stripAnnot vs)
        VSet  t vs      -> VSet       (stripAnnot t)  vs
        VMap  tk tv kvs -> VMap       (stripAnnot tk) (stripAnnot tv) (Map.map stripAnnot kvs)
        VClosure clo    -> VClosure   (stripAnnot clo)
        VBundle bb      -> VBundle    (stripAnnot bb)
        VLoc t i        -> VLoc       (stripAnnot t) i
        VAddr a         -> VAddr a
        VPtr r t a      -> VPtr       (stripAnnot r)  (stripAnnot t) a
        VExtPair v ts a -> VExtPair   (stripAnnot v)  (map stripAnnot ts) (stripAnnot a)


instance StripAnnot TermClosure where
 stripAnnot (TermClosure env mps m)
  = TermClosure (stripAnnot env) (stripAnnot mps) (stripAnnot m)


instance StripAnnot TermEnv where
 stripAnnot (TermEnv bs)
  = TermEnv (map stripAnnot bs)


instance StripAnnot TermEnvBinds where
 stripAnnot eb
  = case eb of
        TermEnvTypes  ts    -> TermEnvTypes     (Map.map stripAnnot ts)
        TermEnvValues vs    -> TermEnvValues    (Map.map stripAnnot vs)
        TermEnvValuesRec vs -> TermEnvValuesRec (Map.map stripAnnot vs)


instance StripAnnot Bundle where
 stripAnnot (Bundle nts nms)
  = Bundle (Map.map stripAnnot nts)
           (Map.map stripAnnot nms)


instance StripAnnot BundleType where
 stripAnnot (BundleType _a n tps k t)
  = BundleType () n (map stripAnnot tps) (stripAnnot k) (stripAnnot t)


instance StripAnnot BundleTerm where
 stripAnnot (BundleTerm _a n tgs ts m)
  = BundleTerm () n (map stripAnnot tgs) (map stripAnnot ts) (stripAnnot m)

