
module Salt.Core.Check.Type where
import Salt.Core.Check.Context
import Salt.Core.Check.Eq
import Salt.Core.Check.Kind
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Transform.MapAnnot
import Salt.Core.Exp
import qualified Salt.Core.Prim.Ctor    as Prim
import qualified Salt.Data.List         as List

import Control.Monad
import Control.Exception
import qualified Data.Map               as Map


---------------------------------------------------------------------------------------------------
-- | Check and elaborate a type producing, a new type and its kind.
--   Type errors are thrown as exceptions in the IO monad.
checkType :: Annot a => a -> [Where a]
          -> Context a -> Type a -> IO (Type a, Kind a)

-- (k-ann) ------------------------------------------------
checkType _a wh ctx (TAnn a' t)
 = checkType a' wh ctx t


-- (k-hole) -----------------------------------------------
checkType _a _wh _ctx THole
 = return (THole, TData)


-- (k-prm) ------------------------------------------------
checkType a wh _ctx t@(TRef (TRPrm n))
 = case Map.lookup n Prim.primTypeCtors of
        Just k  -> return (t, mapAnnot (const a) k)
        Nothing -> throw $ ErrorUnknownTypePrim a wh n


-- (k-con) ------------------------------------------------
checkType _a _wh _ctx t@(TRef (TRCon _n))
 = error $ "TODO: check type synonyms" ++ show t


-- (k-var) ------------------------------------------------
checkType a wh ctx t@(TVar u)
 = do   mt <- contextResolveTypeBound u ctx
        case mt of
         Just k  -> return (t, k)
         Nothing -> throw $ ErrorUnknownTypeBound a wh u


-- (k-abs) ------------------------------------------------
checkType a wh ctx (TAbs ps tBody)
 = do   ps'     <- checkTypeParams a wh ctx ps
        let ctx' = contextBindTypeParams ps' ctx
        (tBody', kResult)  <- checkType a wh ctx' tBody
        let TPTypes bks = ps'
        let ksParam     = map snd bks
        return  ( TAbs ps' tBody'
                , TArr ksParam kResult)


-- (k-app) ------------------------------------------------
checkType a wh ctx (TApt tFun tsArg)
 = do   (tFun',  kFun)    <- checkType a wh ctx tFun
        (tsArg', kResult) <- checkTypeAppTypes a wh ctx kFun tsArg
        return  (TApt tFun' tsArg', kResult)


-- (k-all) ------------------------------------------------
checkType a wh ctx (TForall bks tBody)
 = do   TPTypes bks' <- checkTypeParams a wh ctx (TPTypes bks)
        let ctx' = contextBindTypeParams (TPTypes bks') ctx
        tBody'  <- checkTypeIs a wh ctx' tBody TData
        return  (TForall bks' tBody', TData)


-- (k-ext) ------------------------------------------------
checkType a wh ctx (TExists bks tBody)
 = do   TPTypes bks' <- checkTypeParams a wh ctx (TPTypes bks)
        let ctx' = contextBindTypeParams (TPTypes bks') ctx
        tBody'  <- checkTypeIs a wh ctx' tBody TData
        return  (TExists bks' tBody', TData)


-- (k-fun) ------------------------------------------------
checkType a wh ctx (TFun tsParam tsResult)
 = do
        tsParam'  <- checkTypesAre a wh ctx tsParam
                  $  replicate (length tsParam)  TData

        tsResult' <- checkTypesAre a wh ctx tsResult
                  $  replicate (length tsResult) TData

        return  (TFun tsParam' tsResult', TData)


-- (k-rec) ------------------------------------------------
checkType a wh ctx (TRecord ns tgsField)
 = do
        let nsDup = List.duplicates ns
        when (not $ null nsDup)
         $ throw $ ErrorRecordTypeDuplicateFields a wh nsDup

        tgsField' <- mapM (checkTypeArgsAreAll a wh ctx TData) tgsField
        return  (TRecord ns tgsField', TData)


-- (k-vnt) ------------------------------------------------
checkType a wh ctx (TVariant ns tgsField)
 = do
        let nsDup = List.duplicates ns
        when (not $ null nsDup)
         $ throw $ ErrorVariantTypeDuplicateAlts a wh nsDup

        tgsField' <- mapM (checkTypeArgsAreAll a wh ctx TData) tgsField
        return  (TVariant ns tgsField', TData)


-- (k-susp) -----------------------------------------------
checkType a wh ctx (TSusp tsResult tsEffect)
 = do
        tsResult' <- checkTypesAre a wh ctx tsResult
                  $  replicate (length tsResult) TData

        tsEffect' <- checkTypesAre a wh ctx tsEffect
                  $  replicate (length tsEffect) TEffect

        return  (TSusp tsResult' tsEffect', TData)


-- (k-pure) -----------------------------------------------
checkType _a _wh _ctx TPure
 = do   return (TPure, TEffect)


-- (k-sync) -----------------------------------------------
checkType _a _wh _ctx TSync
 = do   return (TSync, TEffect)


-- (k-sum) ------------------------------------------------
checkType a wh ctx (TSum ts)
 = do
        ts'     <- checkTypesAre a wh ctx ts
                $  replicate (length ts) TEffect

        return  (TSum ts', TEffect)


-----------------------------------------------------------
-- The type expression is malformed,
--   so we don't have any rule that could match it.
checkType a wh _ t
 = throw $ ErrorTypeMalformed a wh t


---------------------------------------------------------------------------------------------------
-- | Check the kinds of some types.
checkTypes
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> IO ([Type a], [Kind a])
checkTypes a wh ctx ts
 = fmap unzip $ mapM (checkType a wh ctx) ts


-- | Check the kind of a single type matches the expected one.
checkTypeIs
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> Kind a -> IO (Type a)
checkTypeIs a wh ctx t k
 = do   [t']    <- checkTypesAre a wh ctx [t] [k]
        return t'


-- | Check the kinds of some types match the expected ones.
checkTypesAre
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> [Kind a] -> IO [Type a]

checkTypesAre a wh ctx ts ksExpected
 = do   (ts', ksActual) <- checkTypes a wh ctx ts

        if length ts' /= length ksActual
         then throw $ ErrorAppTypeTypeWrongArity a wh ksExpected ksActual
         else case checkTypeEqs a [] ksExpected a [] ksActual of
                Just ((_aErr1', kErr1), (_aErr2, kErr2))
                 -> throw  $ ErrorTypeMismatch a wh kErr1 kErr2

                Nothing -> return ts'


---------------------------------------------------------------------------------------------------
-- | Check some type parameters.
checkTypeParams
        :: Annot a => a -> [Where a]
        -> Context a -> TypeParams a -> IO (TypeParams a)

checkTypeParams a wh ctx tps
 = case tps of
        TPTypes bks
         -> do  let (bs, ks) = unzip bks
                ks' <- mapM (checkKind a wh ctx) ks
                return $ TPTypes $ zip bs ks'


---------------------------------------------------------------------------------------------------
-- | Check some type arguments parameters.
checkTypeArgsAreAll
        :: Annot a => a -> [Where a]
        -> Context a -> Kind a -> TypeArgs a ->  IO (TypeArgs a)

checkTypeArgsAreAll a wh ctx kExpected tgs
 = case tgs of
        TGTypes ts
         -> do  ts' <- checkTypesAre a wh ctx ts
                     $ replicate (length ts) kExpected
                return $ TGTypes ts'


---------------------------------------------------------------------------------------------------
-- | Check the application of a type to some types.
checkTypeAppTypes
        :: Annot a => a -> [Where a]
        -> Context a -> Kind a -> [Type a] -> IO ([Type a], Kind a)

checkTypeAppTypes a wh ctx kFun tsArg
 = case kFun of
        TArr ksParam kResult
          -> goCheckArgs ksParam kResult
        _ -> throw $ ErrorAppTypeTypeCannot a wh kFun
 where
        goCheckArgs ksParam kResult
         = do   if length ksParam /= length tsArg
                 then throw $ ErrorAppTypeTypeWrongArityNum a wh ksParam (length tsArg)
                 else do
                        (tsArg', ksArg) <- checkTypes a wh ctx tsArg
                        goCheckParams ksParam kResult tsArg' ksArg

        goCheckParams ksParam kResult tsArg' ksArg
         = case checkTypeEqs a [] ksParam a [] ksArg of
                Just ((_aErr1', kErr1), (_aErr2, kErr2))
                 -> throw $ ErrorTypeMismatch a wh kErr1 kErr2

                Nothing -> return (tsArg', kResult)

