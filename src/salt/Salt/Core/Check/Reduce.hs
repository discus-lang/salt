
module Salt.Core.Check.Reduce where
import Salt.Core.Check.Where
import Salt.Core.Check.Context
import Salt.Core.Transform.Subst
import Salt.Core.Exp
import qualified Data.Map as Map

---------------------------------------------------------------------------------------------------
-- | Reduce a type to weak head normal form.
--   If the head is also an effect type then normalize it.
reduceType
        :: Annot a => a -> [Where a] -> Context a
        -> Type a -> IO (Type a)

reduceType a wh ctx (TAnn _a t)
 = reduceType a wh ctx t

reduceType a wh ctx (TVar (Bound n))
 | Just (_k, tBody) <- Map.lookup n (contextModuleType ctx)
 = reduceType a wh ctx tBody

reduceType _a _wh _ctx tt@(TSum{})
 = return $ flattenType tt

reduceType a wh ctx tt@(TApt tFun tsArgs)
 = do   tFun' <- reduceType a wh ctx tFun
        case tFun' of
         TAbs (TPTypes bks) tBody
          | length bks == length tsArgs
          -> do let ns    = [n | BindName n <- map fst bks]
                let subst = Map.fromList $ zip ns tsArgs
                reduceType a wh ctx $ substTypeType [subst] tBody

         _ -> return tt

reduceType _ _ _ tt
 = return tt


-- | Reduce all the given types to head normal form.
--   We look through annotations, unfold synonyms and reduce type applications
--   to reveal the head constructor.
reduceTypes
        :: Annot a => a -> [Where a] -> Context a
        -> [Type a] -> IO [Type a]

reduceTypes a wh ctx ts
 = mapM (reduceType a wh ctx) ts


-- | Reduce the expected types in a Mode
reduceMode
        :: Annot a => a -> [Where a] -> Context a
        -> Mode a -> IO (Mode a)

reduceMode a wh ctx mode
 = case mode of
        Check ts
         -> do  ts'     <- reduceTypes a wh ctx ts
                return  $ Check ts'

        _ -> return mode


---------------------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------------------
-- | Strip params.
stripTermParamsOfType
        :: Annot a => Context a -> Type a
        -> IO [Either [(Bind, Kind a)] [Type a]]

stripTermParamsOfType ctx tt
 = case tt of
        TAnn _ t
         -> stripTermParamsOfType ctx t

        TForall bks tBody
         -> do  rest    <- stripTermParamsOfType ctx tBody
                return  $ Left bks : rest

        TFun tsParam [tBody]
         -> do  rest    <- stripTermParamsOfType ctx tBody
                return  $ Right tsParam : rest

        TFun tsParam _tsBody
         -> do  return  $ [Right tsParam]

        _ -> return []

