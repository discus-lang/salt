
module Salt.Core.Check.Type.Base
        ( module Salt.Core.Check.Context
        , module Salt.Core.Check.Eq
        , module Salt.Core.Check.Kind
        , module Salt.Core.Check.Where
        , module Salt.Core.Check.Error
        , module Salt.Core.Transform.MapAnnot
        , module Salt.Core.Exp

        , module Control.Monad
        , module Control.Exception

        , checkType
        , checkTypes
        , checkTypeIs
        , checkTypesAre
        , checkTypesAreAll

        , checkTypeParams
        , checkTypeParamss
        , checkTypeArgsAreAll)
where
import Salt.Core.Check.Context
import Salt.Core.Check.Eq
import Salt.Core.Check.Kind
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Transform.MapAnnot
import Salt.Core.Exp
import qualified Salt.Data.List as List

import Control.Monad
import Control.Exception


---------------------------------------------------------------------------------------------------
-- | Kind check a single type.
checkType :: CheckType a
checkType a wh ctx ty
 = contextCheckType ctx a wh ctx ty


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
        -> Context a -> Kind a -> Type a -> IO (Type a)
checkTypeIs a wh ctx k t
 = do   [t']    <- checkTypesAre a wh ctx [k] [t]
        return t'


-- | Check the kinds of some types match the expected ones.
checkTypesAre
        :: Annot a => a -> [Where a]
        -> Context a -> [Kind a] -> [Type a] -> IO [Type a]

checkTypesAre a wh ctx ksExpected ts
 = do   (ts', ksActual)
         <- checkTypes a wh ctx ts

        when (not $ length ts' == length ksActual)
         $ throw $ ErrorAppTypeTypeWrongArity a wh ksExpected ksActual

        checkTypeEqs ctx a [] ksExpected a [] ksActual
         >>= \case
                Nothing -> return ts'
                Just ((_aErr1', kErr1), (_aErr2, kErr2))
                 -> throw $ ErrorTypeMismatch a wh kErr1 kErr2


-- | Check that some types all have the given kind.
checkTypesAreAll
        :: Annot a => a -> [Where a]
        -> Context a -> Kind a -> [Type a] -> IO [Type a]

checkTypesAreAll a wh ctx kExpected ts
 = checkTypesAre a wh ctx (replicate (length ts) kExpected) ts


---------------------------------------------------------------------------------------------------
-- | Check some type parameters.
checkTypeParams
        :: Annot a => a -> [Where a]
        -> Context a -> TypeParams a -> IO (TypeParams a)

checkTypeParams a wh ctx tps
 = case tps of
        TPTypes bks
         -> do  let (bs, ks) = unzip bks

                -- Check for duplicate binder names.
                let ns          = [ n | BindName n <- bs ]
                let nsDup       = List.duplicates ns
                when (not $ null nsDup)
                 $ throw $ ErrorAbsTypeBindConflict a wh nsDup

                -- Check the parameter kinds.
                ks' <- mapM (checkKind a wh ctx) ks
                return $ TPTypes $ zip bs ks'


-- | Check a list of type function parameters,
--   where type variables bound earlier in the list are in scope
--   when checking types annotating term variables later in the list.
checkTypeParamss
        :: Annot a => a -> [Where a]
        -> Context a -> [TypeParams a] -> IO [TypeParams a]

checkTypeParamss _a _wh _ctx []
 = return []

checkTypeParamss a wh ctx (tps : tpss)
 = do   tps'  <- checkTypeParams  a wh ctx  tps
        let ctx'  = contextBindTypeParams tps' ctx
        tpss' <- checkTypeParamss a wh ctx' tpss
        return $ tps' : tpss'


---------------------------------------------------------------------------------------------------
-- | Check some type arguments parameters.
checkTypeArgsAreAll
        :: Annot a => a -> [Where a]
        -> Context a -> Kind a -> TypeArgs a ->  IO (TypeArgs a)

checkTypeArgsAreAll a wh ctx kExpected tgs
 = case tgs of
        TGTypes ts
         -> do  ts' <- checkTypesAre a wh ctx
                        (replicate (length ts) kExpected)
                        ts

                return $ TGTypes ts'

