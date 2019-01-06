
module Salt.Core.Check.Kind where
import Salt.Core.Check.Context
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Exp
import qualified Salt.Core.Prim.Ctor    as Prim
import qualified Data.Map               as Map
import Control.Exception


-- | Check and elaborate a kind.
--   Type errors are thrown as exceptions in the IO monad.
checkKind :: Annot a => a -> [Where a]
          -> Context a -> Kind a -> IO (Kind a)

-- (s-ann) ------------------------------------------------
checkKind _a wh ctx (TAnn a' t)
 = checkKind a' wh ctx t


-- (s-prm) ------------------------------------------------
checkKind a wh _ctx k@(TRef (TRPrm n))
 = case Map.lookup n Prim.primKindCtors of
        Just () -> return k
        Nothing -> throw $ ErrorUnknownPrim UKind a wh n


-- (s-arr) ------------------------------------------------
checkKind a wh ctx (TArr ks1 k2)
 = do   ks1' <- mapM (checkKind a wh ctx) ks1
        k2'  <- checkKind a wh ctx k2
        return  $ TArr ks1' k2'


-- The kind expression is malformed,
--   so we don't have any rule that could match it.
checkKind a wh _ k
 = throw $ ErrorTypeMalformed UKind a wh k
