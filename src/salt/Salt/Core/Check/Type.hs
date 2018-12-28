
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
 = checkType a' wh ctx t


-- (k-hole) -----------------------------------------------
checkTypeWith _a _wh _ctx THole
 = return (THole, TData)


-- (k-prm) ------------------------------------------------
checkTypeWith a wh _ctx t@(TRef (TRPrm n))
 = case Map.lookup n Prim.primTypeCtors of
        Just k  -> return (t, mapAnnot (const a) k)
        Nothing -> throw $ ErrorUnknownTypePrim a wh n


-- (k-var) ------------------------------------------------
checkTypeWith a wh ctx t@(TVar u)
 = contextResolveTypeBound ctx u
 >>= \case
         Just (k, _) -> return (t, k)
         Nothing     -> throw $ ErrorUnknownTypeBound a wh u


-- (k-abs) ------------------------------------------------
checkTypeWith a wh ctx (TAbs ps tBody)
 = do   ps'@(TPTypes bks)  <- checkTypeParams a wh ctx ps
        let ctx'     = contextBindTypeParams ps' ctx
        (tBody', kResult)  <- checkType a wh ctx' tBody
        let ksParam  = map snd bks
        return  ( TAbs ps' tBody'
                , TArr ksParam kResult)


-- (k-app) ------------------------------------------------
checkTypeWith a wh ctx (TApt tFun tsArg)
 = do   (tFun',  kFun)    <- checkType a wh ctx tFun
        (tsArg', kResult) <- checkTypeAppTypes a wh ctx kFun tsArg
        return  (TApt tFun' tsArg', kResult)


-- (k-all) ------------------------------------------------
checkTypeWith a wh ctx (TForall bks tBody)
 = do   TPTypes bks' <- checkTypeParams a wh ctx (TPTypes bks)
        let ctx' = contextBindTypeParams (TPTypes bks') ctx
        tBody'  <- checkTypeIs a wh ctx' TData tBody
        return  (TForall bks' tBody', TData)


-- (k-ext) ------------------------------------------------
checkTypeWith a wh ctx (TExists bks tBody)
 = do   TPTypes bks' <- checkTypeParams a wh ctx (TPTypes bks)
        let ctx' = contextBindTypeParams (TPTypes bks') ctx
        tBody'  <- checkTypeIs a wh ctx' TData tBody
        return  (TExists bks' tBody', TData)


-- (k-fun) ------------------------------------------------
checkTypeWith a wh ctx (TFun tsParam tsResult)
 = do   tsParam'  <- checkTypesAreAll a wh ctx TData tsParam
        tsResult' <- checkTypesAreAll a wh ctx TData tsResult
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
 = do   tsResult' <- checkTypesAreAll a wh ctx TData tsResult
        tEffect'  <- checkTypeIs a wh ctx TEffect tEffect
        return  (TSusp tsResult' tEffect', TData)


-- (k-sync) -----------------------------------------------
checkTypeWith _a _wh _ctx TSync
 = do   return (TSync, TEffect)


-- (k-pure) -----------------------------------------------
checkTypeWith _a _wh _ctx TPure
 = do   return (TPure, TEffect)


-- (k-sum) ------------------------------------------------
checkTypeWith a wh ctx (TSum ts)
 = do   ts' <- checkTypesAreAll  a wh ctx TEffect ts
        return  (TSum ts', TEffect)


-----------------------------------------------------------
-- The type expression is malformed,
--   so we don't have any rule that could match it.
checkTypeWith a wh _ t
 = throw $ ErrorTypeMalformed a wh t

