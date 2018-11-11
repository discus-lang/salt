
module Salt.Core.Check.Type where
import Salt.Core.Check.Context
import Salt.Core.Check.Eq
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Transform.MapAnnot
import Salt.Core.Exp
import qualified Salt.Core.Prim.Data    as Prim


import Control.Exception
import qualified Data.Map               as Map


---------------------------------------------------------------------------------------------------
-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
checkType :: Annot a => a -> [Where a]
          -> Context a -> Type a -> IO (Type a, Kind a)

-- Ann --------------------------------------------------
checkType _a wh ctx (TAnn a' t)
 = checkType a' wh ctx t

-- Prm --------------------------------------------------
-- TODO: proper error for unknown ctors.
checkType a _wh _ctx t@(TRef (TRPrm n))
 = case Map.lookup n Prim.primTypeCtors of
        Nothing -> error "unknown type ctor"
        Just k  -> return (t, mapAnnot (const a) k)


-- Ref --------------------------------------------------
-- TODO: proper error for unknown synonyms.
checkType _a _wh _ctx t@(TRef (TRCon _n))
 = error $ "type synonyms not done " ++ show t


-- Var --------------------------------------------------
checkType a wh ctx t@(TVar u)
 = do   mt <- contextResolveTypeBound u ctx
        case mt of
         Just k  -> return (t, k)
         Nothing -> throw $ ErrorUnknownTypeBound a wh u


-- Abs --------------------------------------------------
checkType a wh ctx (TAbs ps tBody)
 = do   let ctx' = contextBindTypeParams ps ctx
        (tBody', kResult)  <- checkType a wh ctx' tBody
        let TPTypes bks = ps
        let ksParam     = map snd bks
        return  ( TAbs ps tBody'
                , TArr ksParam kResult)


-- TKApp ------------------------------------------------
-- TODO: reduce type redexes.
checkType a wh ctx (TApt tFun tsArg)
 = do   (tFun',  kFun)    <- checkType a wh ctx tFun
        (tsArg', kResult) <- checkTypeAppTypes a wh ctx kFun tsArg
        return  (TApt tFun' tsArg', kResult)


-- TKForall ---------------------------------------------
checkType a wh ctx (TForall bksParam tBody)
 = do   let ctx' = contextBindTypeParams (TPTypes bksParam) ctx
        tBody'  <- checkTypeIs a wh ctx' tBody TData
        return  (TForall bksParam tBody', TData)


-- TKExists ---------------------------------------------
checkType a wh ctx (TExists bksParam tBody)
 = do   let ctx' = contextBindTypeParams (TPTypes bksParam) ctx
        tBody'  <- checkTypeIs a wh ctx' tBody TData
        return  (TExists bksParam tBody', TData)


-- TKFun ------------------------------------------------
checkType a wh ctx (TFun tsParam tsResult)
 = do
        tsParam'  <- checkTypesAre a wh ctx tsParam
                  $  replicate (length tsParam)  TData

        tsResult' <- checkTypesAre a wh ctx tsResult
                  $  replicate (length tsResult) TData

        return  (TFun tsParam' tsResult', TData)


-- TKRecord -----------------------------------------------
checkType a wh ctx (TRecord ns tsField)
 = do
        -- TODO: check for duplicate field names.
        tsField' <- checkTypesAre a wh ctx tsField
                  $ replicate (length tsField) TData

        return  (TRecord ns tsField', TData)

checkType _ _ _ t
 = error $ show t


---------------------------------------------------------------------------------------------------
-- | Check the kinds of some types.
checkTypes
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> IO ([Type a], [Kind a])
checkTypes a wh ctx ts
 = fmap unzip $ mapM (checkType a wh ctx) ts


-- | Check the kind of a type matches the expected ones.
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

