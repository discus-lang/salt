
module Salt.Core.Check.Type where
import Salt.Core.Check.Context
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Exp

import Control.Exception


---------------------------------------------------------------------------------------------------
-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
checkType :: Annot a => a -> [Where a]
          -> Context a -> Type a -> IO (Type a, [Kind a])
checkType _ _ _ t
 = return (t, [TData])


---------------------------------------------------------------------------------------------------
-- | Like 'checkTerm' but expect a single result type.
checkType1
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> IO (Type a, Kind a)
checkType1 a wh ctx t
 = do   (m', ts') <- checkType a wh ctx t
        case ts' of
         [t']   -> return (m', t')
         _      -> throw $ ErrorTypesWrongArity a wh ts' [TData]


-- | Check a list of individual types.
checkTypes
        :: Annot a => a -> [Where a]
        -> Context a -> [Type a] -> IO ([Type a], [Kind a])
checkTypes a wh ctx ms
 =      fmap unzip $ mapM (checkType1 a wh ctx) ms

