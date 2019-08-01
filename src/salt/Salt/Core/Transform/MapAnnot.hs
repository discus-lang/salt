
module Salt.Core.Transform.MapAnnot where
import Salt.Core.Exp.Module
import Salt.Core.Exp.Type
import Salt.Core.Exp.Term
import qualified Data.Map       as Map


---------------------------------------------------------------------------------------------------
class MapAnnot c where
 mapAnnot :: (a -> b) -> c a -> c b


---------------------------------------------------------------------------------------------------
instance MapAnnot Module where
 mapAnnot f mm
        = Module
        { moduleDecls   = map (mapAnnot f) $ moduleDecls mm }


---------------------------------------------------------------------------------------------------
instance MapAnnot Decl where
 mapAnnot f dd
  = case dd of
        DType d -> DType (mapAnnot f d)
        DTerm d -> DTerm (mapAnnot f d)
        DTest d -> DTest (mapAnnot f d)
        DEmit d -> DEmit (mapAnnot f d)


instance MapAnnot DeclType where
 mapAnnot f (DeclType a n tps kResult tBody)
  = DeclType
        (f a) n
        (map (mapAnnot f) tps)
        (mapAnnot f kResult)
        (mapAnnot f tBody)


instance MapAnnot DeclTerm where
 mapAnnot f (DeclTerm a mode n mps tsResult mBody)
  = DeclTerm
        (f a) mode n
        (map (mapAnnot f) mps)
        (map (mapAnnot f) tsResult)
        (mapAnnot f mBody)


instance MapAnnot DeclTest where
 mapAnnot f dd
  = case dd of
        DeclTestKind     a w mn t   -> DeclTestKind     (f a) w mn (mapAnnot f t)
        DeclTestType     a w mn m   -> DeclTestType     (f a) w mn (mapAnnot f m)
        DeclTestEvalType a w mn m   -> DeclTestEvalType (f a) w mn (mapAnnot f m)
        DeclTestEvalTerm a w mn m   -> DeclTestEvalTerm (f a) w mn (mapAnnot f m)
        DeclTestExec     a w mn m   -> DeclTestExec     (f a) w mn (mapAnnot f m)
        DeclTestAssert   a w mn m   -> DeclTestAssert   (f a) w mn (mapAnnot f m)


instance MapAnnot DeclEmit where
 mapAnnot f dd
  = case dd of
        DeclEmit a mn m -> DeclEmit (f a) mn (mapAnnot f m)


---------------------------------------------------------------------------------------------------
instance MapAnnot Type where
 mapAnnot f tt
  = case tt of
        TAnn a t        -> TAnn (f a) (mapAnnot f t)
        TRef r          -> TRef (mapAnnot f r)
        TVar u          -> TVar u
        TAbs p t        -> TAbs (mapAnnot f p) (mapAnnot f t)
        TKey k ms       -> TKey k (map (mapAnnot f) ms)


instance MapAnnot TypeRef where
 mapAnnot f tr
  = case tr of
        TRPrm n         -> TRPrm n
        TRCon n         -> TRCon n
        TRClo clo       -> TRClo (mapAnnot f clo)


instance MapAnnot TypeParams where
 mapAnnot f pp
  = case pp of
        TPAnn a tps'    -> TPAnn (f a) (mapAnnot f tps')
        TPTypes bts     -> TPTypes [(b, mapAnnot f t) | (b, t) <- bts]


instance MapAnnot TypeArgs where
 mapAnnot f tgs
  = case tgs of
        TGAnn a tgs'    -> TGAnn (f a) (mapAnnot f tgs')
        TGTypes ts      -> TGTypes (map (mapAnnot f) ts)


instance MapAnnot TypeClosure where
 mapAnnot f (TypeClosure env mps m)
  = TypeClosure (mapAnnot f env) (mapAnnot f mps) (mapAnnot f m)


instance MapAnnot TypeEnv where
 mapAnnot f (TypeEnv bs)
  = TypeEnv (map (mapAnnot f) bs)


instance MapAnnot TypeEnvBinds where
 mapAnnot f eb
  = case eb of
        TypeEnvTypes  ts    -> TypeEnvTypes  (Map.map (mapAnnot f) ts)


---------------------------------------------------------------------------------------------------
instance MapAnnot Term where
 mapAnnot f mm
  = case mm of
        MAnn a m        -> MAnn (f a) (mapAnnot f m)
        MRef r          -> MRef (mapAnnot f r)
        MVar u          -> MVar u
        MAbs p m        -> MAbs (mapAnnot f p) (mapAnnot f m)
        MRec bms m      -> MRec (map (mapAnnot f) bms) (mapAnnot f m)
        MKey k gs       -> MKey k (map (mapAnnot f) gs)


instance MapAnnot TermBind where
 mapAnnot f (MBind b mpss tsResult mBody)
  = MBind b
        (map (mapAnnot f) mpss)
        (map (mapAnnot f) tsResult)
        (mapAnnot f mBody)


instance MapAnnot TermRef where
 mapAnnot f tr
  = case tr of
        MRVal v         -> MRVal (mapAnnot f v)
        MRPrm n         -> MRPrm n
        MRCon n         -> MRCon n


instance MapAnnot TermParams where
 mapAnnot f pp
  = case pp of
        MPAnn a mps     -> MPAnn (f a) $ mapAnnot f mps
        MPTerms bms     -> MPTerms [(b , mapAnnot f m) | (b, m) <- bms]
        MPTypes bts     -> MPTypes [(b , mapAnnot f t) | (b, t) <- bts]


instance MapAnnot TermArgs where
 mapAnnot f gg
  = case gg of
        MGAnn a mgs     -> MGAnn (f a) $ mapAnnot f mgs
        MGTerm  m       -> MGTerm  (mapAnnot f m)
        MGTerms ms      -> MGTerms (map (mapAnnot f) ms)
        MGTypes ts      -> MGTypes (map (mapAnnot f) ts)


instance MapAnnot Value where
 mapAnnot f vv
  = case vv of
        VUnit           -> VUnit
        VSymbol n       -> VSymbol n
        VText   t       -> VText   t
        VBool   b       -> VBool   b
        VNat    n       -> VNat    n
        VInt    i       -> VInt    i
        VWord   i       -> VWord   i
        VInt8   i       -> VInt8   i
        VInt16  i       -> VInt16  i
        VInt32  i       -> VInt32  i
        VInt64  i       -> VInt64  i
        VWord8  i       -> VWord8  i
        VWord16 i       -> VWord16 i
        VWord32 i       -> VWord32 i
        VWord64 i       -> VWord64 i
        VData n ts vs   -> VData n    (map (mapAnnot f) ts) (map (mapAnnot f) vs)
        VRecord  nvss   -> VRecord    [ (n, map (mapAnnot f) vs) | (n, vs) <- nvss ]
        VVariant n t vs -> VVariant n (mapAnnot f t)  (map (mapAnnot f) vs)
        VList t vs      -> VList      (mapAnnot f t)  (map (mapAnnot f) vs)
        VSet  t vs      -> VSet       (mapAnnot f t)  vs
        VMap  tk tv kvs -> VMap       (mapAnnot f tk) (mapAnnot f tv) (Map.map (mapAnnot f) kvs)
        VClosure clo    -> VClosure   (mapAnnot f clo)
        VBundle  bun    -> VBundle    (mapAnnot f bun)
        VLoc t i        -> VLoc       (mapAnnot f t) i
        VAddr a         -> VAddr a
        VPtr r t a      -> VPtr       (mapAnnot f r) (mapAnnot f t) a
        VExtPair v ts a -> VExtPair   (mapAnnot f v) (map (mapAnnot f) ts) (mapAnnot f a)


instance MapAnnot TermClosure where
 mapAnnot f (TermClosure env mps m)
  = TermClosure (mapAnnot f env) (mapAnnot f mps) (mapAnnot f m)


instance MapAnnot TermEnv where
 mapAnnot f (TermEnv bs)
  = TermEnv (map (mapAnnot f) bs)


instance MapAnnot TermEnvBinds where
 mapAnnot f eb
  = case eb of
        TermEnvTypes  ts    -> TermEnvTypes     (Map.map (mapAnnot f) ts)
        TermEnvValues vs    -> TermEnvValues    (Map.map (mapAnnot f) vs)
        TermEnvValuesRec vs -> TermEnvValuesRec (Map.map (mapAnnot f) vs)


instance MapAnnot Bundle where
 mapAnnot f (Bundle nts nms)
  = Bundle (Map.map (mapAnnot f) nts)
           (Map.map (mapAnnot f) nms)


instance MapAnnot BundleType where
 mapAnnot f (BundleType a n tps k t)
  = BundleType (f a) n (map (mapAnnot f) tps) (mapAnnot f k) (mapAnnot f t)


instance MapAnnot BundleTerm where
 mapAnnot f (BundleTerm a n tgs ts m)
  = BundleTerm (f a) n
        (map (mapAnnot f) tgs)
        (map (mapAnnot f) ts)
        (mapAnnot f m)

