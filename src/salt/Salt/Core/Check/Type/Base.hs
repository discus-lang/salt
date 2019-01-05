
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

checkTypesAre uni a wh ctx ksExpected ts
 = do   (ts', ksActual)
         <- checkTypes a wh ctx ts

        when (not $ length ts' == length ksActual)
         $ throw $ ErrorAppTypeTypeWrongArity a wh ksExpected ksActual

        checkTypeEquivs ctx a [] ksActual a [] ksExpected
         >>= \case
                Nothing -> return ts'
                Just ((_aErr1', kErr1), (_aErr2, kErr2))
                 -> throw $ ErrorMismatch uni a wh kErr1 kErr2


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

