
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

          -- | Holds types of top-level bindings in the current module.
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
-- | Lookup the parameter and result types of a data constructor.
contextResolveDataCtor :: Name -> Context a -> IO (Maybe (Type ()))
contextResolveDataCtor nCtor _ctx
 = return $ Map.lookup nCtor Prim.primDataCtors


-- | Lookup a bound type variable from the context.
contextResolveTypeBound :: Bound -> Context a -> IO (Maybe (Type a))
contextResolveTypeBound (Bound n) ctx
 = go $ contextLocal ctx
 where
        go []   = return Nothing

        go (ElemTerms _  : rest)
         = go rest

        go (ElemTypes mp : rest)
         = case Map.lookup n mp of
                Just k  -> return $ Just k
                Nothing -> go rest


-- | Lookup a bound term variable from the context.
contextResolveTermBound :: Bound -> Context a -> IO (Maybe (Type a))
contextResolveTermBound (Bound n) ctx
 = goGlobal
 where
        goGlobal
         = case Map.lookup n (contextModuleTerm ctx) of
                Nothing -> goLocal (contextLocal ctx)
                Just t  -> return $ Just t

        goLocal []   = return Nothing

        goLocal (ElemTerms mp : rest)
         = case Map.lookup n mp of
                Just t  -> return $ Just t
                Nothing -> goLocal rest

        goLocal (ElemTypes _ : rest)
         = goLocal rest

