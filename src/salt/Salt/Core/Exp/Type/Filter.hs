
module Salt.Core.Exp.Type.Filter where
import Salt.Core.Exp.Type.Base
import Salt.Core.Exp.Name (Bind)
import Data.Maybe (catMaybes)

-- data type for filtering / rewriting a type
-- each sub-filter can
--     return original type unchanged (Just)
--     return original type modified (Just)
--     return no result (Nothing) to indicate type should be removed, bubbling up
-- if not present (Nothing), then will use default behaviour, recursively working on sub-parts

data TypeFilter a =
    TypeFilter { typeFilter         :: Maybe ( TypeFilter a -> Type a         -> Maybe (Type a)         )
               , typeRefFilter      :: Maybe ( TypeFilter a -> TypeRef a      -> Maybe (TypeRef a)      )
               , typeParamsFilter   :: Maybe ( TypeFilter a -> TypeParams a   -> Maybe (TypeParams a)   )
               , typeArgsFilter     :: Maybe ( TypeFilter a -> TypeArgs a     -> Maybe (TypeArgs a)     )
               , typeClosureFilter  :: Maybe ( TypeFilter a -> TypeClosure a  -> Maybe (TypeClosure a)  )
               , typeEnvFilter      :: Maybe ( TypeFilter a -> TypeEnv a      -> Maybe (TypeEnv a)      )
               , typeEnvBindsFilter :: Maybe ( TypeFilter a -> TypeEnvBinds a -> Maybe (TypeEnvBinds a) )
               }

emptyTypeFilter :: TypeFilter a
emptyTypeFilter = TypeFilter { typeFilter         = Nothing
                             , typeRefFilter      = Nothing
                             , typeParamsFilter   = Nothing
                             , typeArgsFilter     = Nothing
                             , typeClosureFilter  = Nothing
                             , typeEnvFilter      = Nothing
                             , typeEnvBindsFilter = Nothing
                             }

-- Apply a filter to a TypeFilterable.
applyTypeFilter :: TypeFilterable t => TypeFilter a -> t a -> Maybe (t a)
applyTypeFilter tf t = filterWhole tf t

-- Apply a filter to a list of TypeFilterable(s).
-- If any element would be filtered, then we filter out the whole list.
applyTypeFilterAllOrNone :: TypeFilterable t => TypeFilter a -> [t a] -> Maybe [t a]
applyTypeFilterAllOrNone tf ts = let ts' = applyTypeFilterKeepSome tf ts in
               if (length ts == length ts')
                then Just ts'
                else Nothing

-- Apply a filter to a list of TypeFilterable(s).
-- Keeping all non-filtered elements.
applyTypeFilterKeepSome :: TypeFilterable t => TypeFilter a -> [t a] -> [t a]
applyTypeFilterKeepSome tf ts = catMaybes $ map (applyTypeFilter tf) ts

class TypeFilterable t where
 -- Call supplied type filter on whole type.
 -- Will fall back to filterParts if supplied type filter is missing an appropriate filter.
 filterWhole :: (TypeFilter a) -> (t a) -> Maybe (t a)
 -- Call supplied type filter on each sub component of type.
 filterParts :: (TypeFilter a) -> (t a) -> Maybe (t a)

instance TypeFilterable Type where
 filterWhole tf (TAnn a t) =
    case filterWhole tf t of
        Nothing -> Nothing
        Just t' -> Just $ TAnn a t'
 filterWhole tf t =
    case typeFilter tf of
        Nothing   -> filterParts tf t
        Just f -> f tf t
 filterParts tf t
  = case t of
        TAnn a (ty) -> do
            ty' <- filterWhole tf ty
            return $ TAnn a ty'
        TRef tr     -> do
            tr' <- filterWhole tf tr
            return $ TRef tr'
        -- no work to do within a TVar
        -- either filterWhole did all the work, or there is no work to do
        tv@(TVar _) -> Just tv
        TAbs tp ty  -> do
            tp' <- filterWhole tf tp
            ty' <- filterWhole tf ty
            return $ TAbs tp' ty'
        TKey tk tas -> do
            tas' <- applyTypeFilterAllOrNone tf tas
            return $ TKey tk tas'

instance TypeFilterable TypeRef where
 filterWhole tf t =
    case typeRefFilter tf of
        Nothing   -> filterParts tf t
        Just f -> f tf t
 filterParts tf t
  = case t of
        -- we do not descent into names, must use filterWhole if this is needed.
        p@(TRPrm _) -> Just p
        p@(TRCon _) -> Just p
        TRClo clos -> do
            clos' <- filterWhole tf clos
            return $ TRClo clos'

instance TypeFilterable TypeParams where
 filterWhole tf (TPAnn a tp) =
    case filterWhole tf tp of
        Nothing  -> Nothing
        Just tp' -> Just $ TPAnn a tp'
 filterWhole tf t =
    case typeParamsFilter tf of
        Nothing   -> filterParts tf t
        Just f -> f tf t
 filterParts tf t
  = case t of
        TPAnn a tp -> do
            tp' <- filterWhole tf tp
            return $ TPAnn a tp'
        -- TODO FIXME this is keepSome, we most likely want allOrNone
        -- TODO FIXME is it safe to remove individual type params ?
        TPTypes bindings -> case catMaybes $ map (worker tf) bindings of
            [] -> Nothing
            bs -> Just (TPTypes bs)
     where
        worker :: TypeFilter a -> (Bind, Type a) -> Maybe (Bind, Type a)
        worker tf' (b, t') = do
                            t'' <- filterWhole tf' t'
                            return (b, t'')

instance TypeFilterable TypeArgs where
 filterWhole tf (TGAnn a ta) =
    case filterWhole tf ta of
        Nothing  -> Nothing
        Just ta' -> Just $ TGAnn a ta'
 filterWhole tf t =
    case typeArgsFilter tf of
        Nothing   -> filterParts tf t
        Just f -> f tf t
 filterParts tf t
  = case t of
        TGAnn a tg -> do
            tg' <- filterWhole tf tg
            return $ TGAnn a tg'
        -- TODO always allOrNone, or sometimes keepSome?
        TGTypes ts -> do
            ts' <- applyTypeFilterAllOrNone tf ts
            return $ TGTypes ts'

instance TypeFilterable TypeClosure where
 filterWhole tf t =
    case typeClosureFilter tf of
        Nothing   -> filterParts tf t
        Just f -> f tf t
 filterParts tf (TypeClosure te tp t)  = do
    te' <- filterWhole tf te
    tp' <- filterWhole tf tp
    t'  <- filterWhole tf t
    return $ TypeClosure te' tp' t'

instance TypeFilterable TypeEnv where
 filterWhole tf t =
    case typeEnvFilter tf of
        Nothing   -> filterParts tf t
        Just f -> f tf t
 filterParts tf (TypeEnv tbs) = do
    tbs' <- applyTypeFilterAllOrNone tf tbs
    return $ TypeEnv tbs'

instance TypeFilterable TypeEnvBinds where
 filterWhole tf t =
    case typeEnvBindsFilter tf of
        Nothing   -> filterParts tf t
        Just f -> f tf t
 --filterParts tf (TypeEnvTypes map) = do ???
 -- we do not look inside, must use filterWhole if caller wants to.
 filterParts _ teb = Just teb


