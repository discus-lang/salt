
module Salt.Core.Check.Type.Params where
import Salt.Core.Check.Context
import Salt.Core.Check.Kind
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Exp
import qualified Salt.Data.List as List

import Control.Monad
import Control.Exception


-- | Check some type parameters.
checkTypeParams
        :: Annot a => a -> [Where a]
        -> Context a -> TypeParams a -> IO (TypeParams a)

checkTypeParams _a wh ctx (TPAnn a' tps')
 = checkTypeParams a' wh ctx tps'

checkTypeParams a wh ctx (TPTypes bks)
 = do   let (bs, ks) = unzip bks

        -- Check for duplicate binder names.
        let ns          = [ n | BindName n <- bs ]
        let nsDup       = List.duplicates ns
        when (not $ null nsDup)
         $ throw $ ErrorAbsConflict UType a wh nsDup

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

