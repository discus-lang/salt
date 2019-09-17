
module Salt.Core.Check.Type.Base
        ( module Salt.Core.Check.Context
        , module Salt.Core.Check.Equiv
        , module Salt.Core.Check.Kind
        , module Salt.Core.Check.Where
        , module Salt.Core.Check.Error
        , module Salt.Core.Transform.MapAnnot
        , module Salt.Core.Exp

        , module Control.Monad
        , module Control.Exception
        , module Data.Maybe

        , checkType
        , checkTypes
        , checkTypeHas
        , checkTypesAre
        , checkTypesAreAll

        , checkTypeArgsAreAll)
where
import Salt.Core.Check.Context
import Salt.Core.Check.Equiv
import Salt.Core.Check.Kind
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Transform.MapAnnot
import Salt.Core.Exp

import Control.Monad
import Control.Exception
import Data.Maybe

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
checkTypeHas
        :: Annot a => Universe -> a -> [Where a]
        -> Context a -> Kind a -> Type a -> IO (Type a)

checkTypeHas uni _a wh ctx k (TAnn a' t')
 = do   t''  <- checkTypeHas uni a' wh ctx k t'
        return $ TAnn a' t''

checkTypeHas uni a wh ctx k t
 = do   [t']    <- checkTypesAre uni a wh ctx [k] [t]
        return t'


-- | Check the kinds of some types match the expected ones.
checkTypesAre
        :: Annot a => Universe -> a -> [Where a]
        -> Context a -> [Kind a] -> [Type a] -> IO [Type a]

checkTypesAre uni@UKind a wh ctx ksExpected ts
 = do   (ts', ksActual)
         <- checkTypes a wh ctx ts

        when (not $ length ksExpected == length ksActual)
         $ throw $ ErrorAppTypeTypeWrongArity a wh ksExpected ksActual

        -- Check each of the types in turn against their expected kinds.
        -- If one of them doesn't match then attribute the error to the
        -- top most annotatino on the type that was checked, not the
        -- resulting kind expression.
        forM (zip3 ts' ksActual ksExpected) $ \(t', kActual, kExpected)
         -> do  let a' = fromMaybe a $ takeAnnotOfType t'
                checkTypeEquiv ctx a' [] kActual a' [] kExpected
                 >>= \case
                        Nothing -> return t'
                        Just ((_aErr1', kErr1), (_aErr2, kErr2))
                         -> throw $ ErrorMismatch uni a' wh kErr1 kErr2


checkTypesAre UType a wh ctx ksExpected ts
 = do   (ts', ksActual)
         <- checkTypes a wh ctx ts

        when (not $ length ksExpected == length ksActual)
         $ throw $ ErrorAppTypeTypeWrongArity a wh ksExpected ksActual

        -- Check each of the types in turn against their expected kinds.
        -- If one of them doesn't match then attribute the error to the
        -- top most annotatino on the type that was checked, not the
        -- resulting kind expression.
        forM (zip3 ts' ksActual ksExpected) $ \(t', kActual, kExpected)
         -> do  let a' = fromMaybe a $ takeAnnotOfType t'
                checkKindEquiv a' wh kActual kExpected
                return t'

-- This function should only be called at the UKind and UType universes.
-- If not then the caller is very broken.
checkTypesAre _uni _ _ _ _ _
 = error "universe malfunction"


-- | Check kinds first (actual) kind is a subkind of the second (expected) one.
checkKindEquiv
        :: Annot a => a -> [Where a]
        -> Kind a -> Kind a
        -> IO ()

checkKindEquiv _ wh (TAnn a' t') kExpected
 = checkKindEquiv a' wh t' kExpected

checkKindEquiv _ wh kActual (TAnn a' t')
 = checkKindEquiv a' wh kActual t'

checkKindEquiv a wh kActual kExpected
 = case (kActual, kExpected) of
        (TRepr,   TRepr)        -> return ()
        (TData,   TData)        -> return ()
        (TComp,   TComp)        -> return ()
        (TProp,   TProp)        -> return ()
        (TRegion, TRegion)      -> return ()
        (TEffect, TEffect)      -> return ()

        (TComp,   TRepr)        -> return ()
        (TData,   TRepr)        -> return ()

        _ -> throw $ ErrorMismatch UKind a wh kActual kExpected


-- | Check that some types all have the given kind.
checkTypesAreAll
        :: Annot a => Universe -> a -> [Where a]
        -> Context a -> Kind a -> [Type a] -> IO [Type a]

checkTypesAreAll uni a wh ctx kExpected ts
 = checkTypesAre uni a wh ctx (replicate (length ts) kExpected) ts


---------------------------------------------------------------------------------------------------
-- | Check some type arguments parameters.
checkTypeArgsAreAll
        :: Annot a => a -> [Where a]
        -> Context a -> Kind a -> TypeArgs a ->  IO (TypeArgs a)

checkTypeArgsAreAll a wh ctx kExpected (TGAnn _ tgs')
 = checkTypeArgsAreAll a wh ctx kExpected tgs'

checkTypeArgsAreAll a wh ctx kExpected (TGTypes ts)
 = do   ts' <- checkTypesAre UKind a wh ctx
                (replicate (length ts) kExpected)
                ts

        return $ TGTypes ts'

