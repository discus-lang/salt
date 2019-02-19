
module Salt.Core.Check.Term.Params where
import Salt.Core.Check.Kind
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import qualified Salt.Data.List as List


-- | Check some term function parameters.
checkTermParams
        :: Annot a => a -> [Where a]
        -> Context a -> TermParams a -> IO (TermParams a)

checkTermParams _a wh ctx (MPAnn a' mps)
 = checkTermParams a' wh ctx mps

checkTermParams a wh ctx (MPTypes bks)
 = do   let (bs, ks) = unzip bks

        -- Check for duplicate binder names.
        let ns    = [ n | BindName n <- bs]
        let nsDup = List.duplicates ns
        when (not $ null nsDup)
         $ throw $ ErrorAbsConflict UType a wh nsDup

        -- Check the parameter kinds.
        ks' <- mapM (checkKind a wh ctx) ks
        return  $ MPTypes $ zip bs ks'

checkTermParams a wh ctx (MPTerms bts)
 = do   let (bs, ts) = unzip bts

        -- Check for duplicate binder names.
        let ns    = [n | BindName n <- bs]
        let nsDup = List.duplicates ns
        when (not $ null nsDup)
         $ throw $ ErrorAbsConflict UTerm a wh nsDup

        -- Check the parameter types.
        ts' <- checkTypesAre UType a wh ctx (replicate (length ts) TData) ts
        return  $ MPTerms $ zip bs ts'


-- | Check a list of term function parameters,
--   where type variables bound earlier in the list are in scope
--   when checking types annotating term variables later in the list.
checkTermParamss
        :: Annot a => a -> [Where a]
        -> Context a -> [TermParams a] -> IO (Context a, [TermParams a])

checkTermParamss _a _wh ctx []
 = return (ctx, [])

checkTermParamss a wh ctx (tps : tpss)
 = do   tps'  <- checkTermParams  a wh ctx  tps
        let ctx'  = contextBindTermParams tps' ctx
        (ctx'', tpss') <- checkTermParamss a wh ctx' tpss
        return (ctx'', tps' : tpss')

