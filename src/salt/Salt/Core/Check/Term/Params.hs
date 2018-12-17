
module Salt.Core.Check.Term.Params where
import Salt.Core.Check.Kind
import Salt.Core.Check.Type
import Salt.Core.Check.Term.Base


-- | Check some term function parameters.
checkTermParams
        :: Annot a => a -> [Where a]
        -> Context a -> TermParams a -> IO (TermParams a)

checkTermParams a wh ctx mps
 = case mps of
        MPTerms bts
         -> do  let (bs, ts) = unzip bts
                ts' <- checkTypesAre a wh ctx ts (replicate (length ts) TData)
                return  $ MPTerms $ zip bs ts'

        MPTypes bks
         -> do  let (bs, ks) = unzip bks
                ks' <- mapM (checkKind a wh ctx) ks
                return  $ MPTypes $ zip bs ks'


-- | Check a list of term function parameters,
--   where type variables bound earlier in the list are in scope
--   when checking types annotating term variables later in the list.
checkTermParamss
        :: Annot a => a -> [Where a]
        -> Context a -> [TermParams a] -> IO [TermParams a]

checkTermParamss _a _wh _ctx []
 = return []

checkTermParamss a wh ctx (tps : tpss)
 = do   tps'  <- checkTermParams  a wh ctx  tps
        let ctx'  = contextBindTermParams tps' ctx
        tpss' <- checkTermParamss a wh ctx' tpss
        return $ tps' : tpss'

