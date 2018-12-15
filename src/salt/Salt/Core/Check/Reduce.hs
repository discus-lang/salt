
module Salt.Core.Check.Reduce where
import Salt.Core.Check.Where
import Salt.Core.Check.Context
import Salt.Core.Exp


-- | Reduce all the given types to head normal form.
--   We look through annotations, unfold synonyms and reduce type applications
--   to reveal the head constructor.
reduceTypes
        :: Annot a => a -> [Where a] -> Context a
        -> [Type a] -> IO [Type a]

reduceTypes a wh ctx ts
 = mapM (reduceType a wh ctx) ts


-- | Reduce a type to weak head normal form.
--   If the head is also an effect type then normalize it.
reduceType
        :: Annot a => a -> [Where a] -> Context a
        -> Type a -> IO (Type a)

reduceType a wh ctx (TAnn _a t)
 = reduceType a wh ctx t

reduceType _a _wh _ctx tt@(TSum{})
 = return $ flattenType tt

reduceType _ _ _ tt
 = return tt


-- | Flatten nested TSums in a type.
flattenType :: Type a -> Type a
flattenType tt
 = case parts tt of
        []      -> TPure
        [t]     -> t
        ts      -> TSum ts
 where
        parts (TSum ts) = concatMap parts ts
        parts TPure     = []
        parts t         = [t]

