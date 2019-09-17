
module Salt.Core.Check.Type where
import Salt.Core.Check.Type.App
import Salt.Core.Check.Type.Params
import Salt.Core.Check.Type.Base
import qualified Salt.Core.Prim.Ctor    as Prim
import qualified Salt.Data.List         as List
import qualified Data.Map               as Map


-- | Check and elaborate a type producing, a new type and its kind.
--   Type errors are thrown as exceptions in the IO monad.
checkTypeWith :: CheckType a

-- (k-ann) ------------------------------------------------
checkTypeWith _a wh ctx (TAnn a' t)
 = do   (t', k) <- checkType a' wh ctx t
        return (TAnn a' t', k)


-- (k-hole) -----------------------------------------------
checkTypeWith _a _wh _ctx THole
 = return (THole, TData)


-- (k-prm) ------------------------------------------------
checkTypeWith a wh _ctx t@(TRef (TRPrm n))
 = case Map.lookup n Prim.primTypeCtors of
        Just k  -> return (t, mapAnnot (const a) k)
        Nothing -> throw $ ErrorUnknownPrim UType a wh n


-- (k-var) ------------------------------------------------
checkTypeWith a wh ctx t@(TVar u)
 = contextResolveTypeBound ctx [] u
 >>= \case
         Just (TypeDecl  k _) -> return (t, k)
         Just (TypeLocal k _) -> return (t, k)
         _ -> throw $ ErrorUnknownBound UType a wh u


-- (k-abs) ------------------------------------------------
checkTypeWith a wh ctx (TAbs ps tBody)
 = do
        ps'@(TPTypes bks)  <- checkTypeParams a wh ctx ps
        let ctx' = contextBindTypeParams ps' ctx
        (tBody', kResult)  <- checkType a wh ctx' tBody
        let ksParam  = map snd bks
        return  ( TAbs ps' tBody'
                , TArr ksParam kResult)


-- (k-arr) ------------------------------------------------
checkTypeWith a wh ctx (TArr ks1 k2)
 = do   ks1'    <- mapM (checkKind a wh ctx) ks1
        k2'     <- checkKind a wh ctx k2
        return  (TArr ks1' k2', TType)


-- (k-app) ------------------------------------------------
checkTypeWith a wh ctx (TApp tFun tgsArg)
 = do   let aFun = fromMaybe a $ takeAnnotOfType tFun
        (tFun',  kFun)     <- checkType a wh ctx tFun
        (tgsArg', kResult) <- checkTypeAppTypes a wh ctx aFun kFun tgsArg
        return  (TApp tFun' tgsArg', kResult)


-- (k-all) ------------------------------------------------
checkTypeWith a wh ctx (TForall tps tBody)
 = do   tps'    <- checkTypeParams a wh ctx tps
        let ctx' = contextBindTypeParams tps' ctx
        tBody'  <- checkTypeHas UType a wh ctx' TData tBody
        return  (TForall tps' tBody', TData)


-- (k-ext) ------------------------------------------------
checkTypeWith a wh ctx (TExists tps tBody)
 = do   tps'    <- checkTypeParams a wh ctx tps
        let ctx' = contextBindTypeParams tps' ctx
        tBody'  <- checkTypeHas UType a wh ctx' TData tBody
        return  (TExists tps' tBody', TData)


-- (k-fun) ------------------------------------------------
checkTypeWith a wh ctx (TFun tsParam tsResult)
 = do   tsParam'  <- checkTypesAreAll UType a wh ctx TData tsParam
        tsResult' <- checkTypesAreAll UType a wh ctx TRepr tsResult
        return  (TFun tsParam' tsResult', TData)


-- (k-rec) ------------------------------------------------
checkTypeWith a wh ctx (TRecord ns tgsField)
 = do   let nsDup = List.duplicates ns
        when (not $ null nsDup)
         $ throw $ ErrorRecordTypeDuplicateFields a wh nsDup

        tgsField' <- mapM (checkTypeArgsAreAll a wh ctx TData) tgsField
        return  (TRecord ns tgsField', TData)


-- (k-vnt) ------------------------------------------------
checkTypeWith a wh ctx (TVariant ns tgsField)
 = do   let nsDup = List.duplicates ns
        when (not $ null nsDup)
         $ throw $ ErrorVariantTypeDuplicateAlts a wh nsDup

        tgsField' <- mapM (checkTypeArgsAreAll a wh ctx TData) tgsField
        return  (TVariant ns tgsField', TData)


-- (k-susp) -----------------------------------------------
checkTypeWith a wh ctx (TSusp tsResult tEffect)
 = do   tsResult' <- checkTypesAreAll UType a wh ctx TData tsResult
        tEffect'  <- checkTypeHas UType a wh ctx TEffect tEffect
        return  (TSusp tsResult' tEffect', TComp)


-- (k-sync) -----------------------------------------------
checkTypeWith _a _wh _ctx TSync
 = do   return (TSync, TEffect)


-- (k-pure) -----------------------------------------------
checkTypeWith _a _wh _ctx TPure
 = do   return (TPure, TEffect)


-- (k-sum) ------------------------------------------------
checkTypeWith a wh ctx (TSum ts)
 = do   ts' <- checkTypesAreAll UType a wh ctx TEffect ts
        return  (TSum ts', TEffect)


-----------------------------------------------------------
-- The type expression is malformed,
--   so we don't have any rule that could match it.
checkTypeWith a wh _ t
 = throw $ ErrorTypeMalformed UType a wh t

