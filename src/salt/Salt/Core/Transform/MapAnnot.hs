
module Salt.Core.Transform.MapAnnot where
import Salt.Core.Exp.Module
import Salt.Core.Exp.Type
import Salt.Core.Exp.Term
import qualified Data.Map       as Map


---------------------------------------------------------------------------------------------------
class MapAnnot c where
 mapAnnot :: (a -> b) -> c a -> c b


-- | Replace all annotations in a thing by the unit value.
stripAnnot :: MapAnnot c => c a -> c ()
stripAnnot xx
 = mapAnnot (const ()) xx


instance MapAnnot Module where
 mapAnnot f mm
        = Module
        { moduleDecls   = map (mapAnnot f) $ moduleDecls mm }


---------------------------------------------------------------------------------------------------
instance MapAnnot Decl where
 mapAnnot f dd
  = case dd of
        DTest  d        -> DTest  (mapAnnot f d)


instance MapAnnot DeclTest where
 mapAnnot f dd
  = case dd of
        DeclTestKind a mn t
         -> DeclTestKind (f a) mn (mapAnnot f t)

        DeclTestType a mn m
         -> DeclTestType (f a) mn (mapAnnot f m)

        DeclTestEval a mn m
         -> DeclTestEval (f a) mn (mapAnnot f m)

        DeclTestAssert a mn m
         -> DeclTestAssert (f a) mn (mapAnnot f m)


---------------------------------------------------------------------------------------------------
instance MapAnnot Type where
 mapAnnot f tt
  = case tt of
        TAnn a t        -> TAnn (f a) (mapAnnot f t)
        TRef r          -> TRef r
        TVar u          -> TVar u
        TAbs p t        -> TAbs (mapAnnot f p) (mapAnnot f t)
        TKey k ms       -> TKey k (map (mapAnnot f) ms)


instance MapAnnot TypeParams where
 mapAnnot f pp
  = case pp of
        TPTypes bts     -> TPTypes [(b, mapAnnot f t) | (b, t) <- bts]


instance MapAnnot TypeArgs where
 mapAnnot f gg
  = case gg of
        TGTypes ts      -> TGTypes (map (mapAnnot f) ts)


---------------------------------------------------------------------------------------------------
instance MapAnnot Term where
 mapAnnot f mm
  = case mm of
        MAnn a m        -> MAnn (f a) (mapAnnot f m)
        MRef r          -> MRef (mapAnnot f r)
        MVar u          -> MVar u
        MAbs p m        -> MAbs (mapAnnot f p) (mapAnnot f m)
        MKey k gs       -> MKey k (map (mapAnnot f) gs)


instance MapAnnot TermRef where
 mapAnnot f tr
  = case tr of
        MRVal v         -> MRVal (mapAnnot f v)
        MRTop ns u      -> MRTop ns u


instance MapAnnot TermParams where
 mapAnnot f pp
  = case pp of
        MPTerms bms     -> MPTerms [(b , mapAnnot f m) | (b, m) <- bms]
        MPTypes bts     -> MPTypes [(b , mapAnnot f t) | (b, t) <- bts]


instance MapAnnot TermArgs where
 mapAnnot f gg
  = case gg of
        MGTerms ms      -> MGTerms (map (mapAnnot f) ms)
        MGTypes ts      -> MGTypes (map (mapAnnot f) ts)


instance MapAnnot Value where
 mapAnnot f vv
  = case vv of
        VUnit           -> VUnit
        VSymbol n       -> VSymbol n
        VText t         -> VText t
        VBool b         -> VBool b
        VInt i          -> VInt i
        VNat n          -> VNat n
        VData n vs      -> VData n (map (mapAnnot f) vs)
        VRecord nvs     -> VRecord [ (n, mapAnnot f v) | (n, v) <- nvs ]
        VList vs        -> VList  $ map (mapAnnot f) vs
        VSet  vs        -> VSet vs
        VMap  kvs       -> VMap   $ Map.map (mapAnnot f) kvs
        VClosure clo    -> VClosure (mapAnnot f clo)


instance MapAnnot Closure where
 mapAnnot f (CloTerm env bs m)
  = CloTerm (mapAnnot f env) (map (mapAnnot f) bs) (mapAnnot f m)


instance MapAnnot Env where
 mapAnnot f (Env nvs)
  = Env [ (n, mapAnnot f v) | (n, v) <- nvs ]

