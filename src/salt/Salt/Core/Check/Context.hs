
module Salt.Core.Check.Context where
import Salt.Core.Exp
import qualified Salt.Core.Prim.Data    as Prim

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
        { -- | Holds types of top-level bindings in the current module.
          contextModuleTerm     :: Map Name (Type a)

          -- | Holds types of local name bindings.
          --   This is used for bindings within a single top-level declaration.
        , contextLocal          :: [Elem a]
        }


-- | Construct an empty context.
contextEmpty :: Context a
contextEmpty
        = Context
        { contextModuleTerm     = Map.empty
        , contextLocal          = [] }


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
contextBindTermParams tps ctx
 = case tps of
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


-- | Lookup a bound term variable from the context.
contextResolveTermBound :: Bound -> Context a -> IO (Maybe (Type a))
contextResolveTermBound (Bound n) ctx
 = go $ contextLocal ctx
 where
        go []   = return Nothing

        go (ElemTerms mp : rest)
         = case Map.lookup n mp of
                Just t  -> return $ Just t
                Nothing -> go rest

        go (ElemTypes _ : rest)
         = go rest

