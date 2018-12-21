
module Salt.Core.Check.Context where
import Salt.Core.Check.Where
import Salt.Core.Exp
import qualified Salt.Core.Prim.Ctor    as Prim

import Data.Map.Strict                  (Map)
import qualified Data.Map               as Map

---------------------------------------------------------------------------------------------------
data Mode a
        = Synth
        | Check [Type a]
        deriving Show


---------------------------------------------------------------------------------------------------
data Elem a
        = ElemTypes (Map Name (Type a))
        | ElemTerms (Map Name (Type a))
        deriving Show


data Context a
        = Context
        { -- | Function to check a type.
          --   We hold a reference to the checker here o tie the mutually recursive
          --   knot without needing mutually recursive modules.
          contextCheckType      :: CheckType a

          -- | Function to check a term.
        , contextCheckTerm      :: CheckTerm a

          -- | Kinds and bodies of top-level type bindings in the current module.
        , contextModuleType     :: Map Name (Kind a, Type a)

          -- | Types of top-level term bindings in the current module.
        , contextModuleTerm     :: Map Name (Type a)

          -- | Holds types of local name bindings.
          --   This is used for bindings within a single top-level declaration.
        , contextLocal          :: [Elem a]
        }


type CheckType a
        =  Annot a => a -> [Where a]
        -> Context a -> Type a
        -> IO (Type a, Kind a)


type CheckTerm a
        =  Annot a => a -> [Where a]
        -> Context a -> Mode a -> Term a
        -> IO (Term a, [Type a], [Effect a])


---------------------------------------------------------------------------------------------------
-- | Bind a single type variable into the context.
contextBindType :: Name -> Type a -> Context a -> Context a
contextBindType n t ctx
 = ctx  { contextLocal = ElemTypes (Map.singleton n t) : contextLocal ctx }


-- | Bind a single type variable into the context, if we have one.
contextBindTypeMaybe :: Maybe Name -> Type a -> Context a -> Context a
contextBindTypeMaybe Nothing _t ctx = ctx
contextBindTypeMaybe (Just n) t ctx
 = ctx  { contextLocal = ElemTypes (Map.singleton n t) : contextLocal ctx }


-- | Bind the kinds of type parameters into the context.
--   TODO: prevent type variable shadowing.
contextBindTypeParams :: TypeParams a -> Context a -> Context a
contextBindTypeParams tps ctx
 = case tps of
        TPTypes bts
         -> let nts = [ (n, t) | (BindName n, t) <- bts ]
            in  ctx { contextLocal = ElemTypes (Map.fromList nts) : contextLocal ctx }


---------------------------------------------------------------------------------------------------
-- | Bind a single term variable into the context.
contextBindTerm :: Name -> Type a -> Context a -> Context a
contextBindTerm n t ctx
 = ctx  { contextLocal = ElemTerms (Map.singleton n t) : contextLocal ctx }


-- | Bind a single term variable into the context, if we have one.
contextBindTermMaybe :: Maybe Name -> Type a -> Context a -> Context a
contextBindTermMaybe Nothing _t ctx = ctx
contextBindTermMaybe (Just n) t ctx
 = ctx  { contextLocal = ElemTerms (Map.singleton n t) : contextLocal ctx }


-- | Bind the types of term parameters into the context.
contextBindTermParams :: TermParams a -> Context a -> Context a
contextBindTermParams mps ctx
 = case mps of
        MPTerms bts
         -> let nts = [ (n, t) | (BindName n, t) <- bts ]
            in  ctx { contextLocal = ElemTerms (Map.fromList nts) : contextLocal ctx }

        MPTypes bts
         -> let nts = [ (n, t) | (BindName n, t) <- bts ]
            in  ctx { contextLocal = ElemTypes (Map.fromList nts) : contextLocal ctx }


---------------------------------------------------------------------------------------------------
-- | Lookup a bound type variable from the context.
--   If it is transparanetly bound as a synonym we get both the kind and body type,
--   If it is opaquely bound by an abstraction we get just the kind.
contextResolveTypeBound :: Bound -> Context a -> IO (Maybe (Kind a, Maybe (Type a)))
contextResolveTypeBound (Bound n) ctx
 = goGlobal
 where
        goGlobal
         = case Map.lookup n (contextModuleType ctx) of
                Nothing     -> goLocal (contextLocal ctx)
                Just (k, t) -> return $ Just (k, Just t)

        goLocal [] = return Nothing

        goLocal (ElemTerms _  : rest)
         = goLocal rest

        goLocal (ElemTypes mp : rest)
         = case Map.lookup n mp of
                Just k  -> return $ Just (k, Nothing)
                Nothing -> goLocal rest


-- | Lookup a bound term variable from the context.
contextResolveTermBound :: Bound -> Context a -> IO (Maybe (Type a))
contextResolveTermBound (Bound n) ctx
 = goGlobal
 where
        goGlobal
         = case Map.lookup n (contextModuleTerm ctx) of
                Nothing -> goLocal (contextLocal ctx)
                Just t  -> return $ Just t

        goLocal [] = return Nothing

        goLocal (ElemTerms mp : rest)
         = case Map.lookup n mp of
                Just t  -> return $ Just t
                Nothing -> goLocal rest

        goLocal (ElemTypes _ : rest)
         = goLocal rest


-- | Lookup the parameter and result types of a data constructor.
contextResolveDataCtor :: Name -> Context a -> IO (Maybe (Type ()))
contextResolveDataCtor nCtor _ctx
 = return $ Map.lookup nCtor Prim.primDataCtors

