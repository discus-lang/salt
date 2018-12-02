
module Salt.Core.Check.Term where
import Salt.Core.Check.Eq
import Salt.Core.Check.Type
import Salt.Core.Check.Context
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Transform.MapAnnot
import Salt.Core.Transform.Subst
import Salt.Core.Exp
import qualified Salt.Core.Prim.Ops     as Prim
import qualified Salt.Core.Prim.Data    as Prim
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set

import Salt.Core.Codec.Text             ()
import qualified Salt.Data.Pretty               as P

import Control.Exception
import Control.Monad
import Data.Maybe

import Text.Show.Pretty

---------------------------------------------------------------------------------------------------
-- | Check and elaborate a term producing, a new term and its type.
--   Type errors are thrown as exceptions in the IO monad.
checkTerm
        :: Annot a => a -> [Where a] -> Context a
        -> Term a -> Mode a -> IO (Term a, [Type a])

-- Ann ----------------------------------------------------
checkTerm _a wh ctx (MAnn a' m) mode
 = checkTerm a' wh ctx m mode

-- (t-mmm) ------------------------------------------------
checkTerm a wh ctx (MTerms msArg) mode
 = do   (msArg', tsArg) <- checkTerms a wh ctx msArg mode
        return  (MTerms msArg', tsArg)


-- (t-the) ------------------------------------------------
checkTerm a wh ctx (MThe ts m) Synth
 = do   (m', _) <- checkTerm a wh ctx m (Check ts)
        return  (MThe ts m', ts)


-- (t-val) ------------------------------------------------
checkTerm _a _wh _ctx m@(MRef (MRVal v)) Synth
 = case v of
        VUnit     -> return (m, [TUnit])
        VSymbol{} -> return (m, [TSymbol])
        VText{}   -> return (m, [TText])
        VBool{}   -> return (m, [TBool])
        VInt{}    -> return (m, [TInt])
        VNat{}    -> return (m, [TNat])
        (VNone t) -> return (m, [TOption t])
        _         -> error "check value not done yet"


-- (t-prm) ------------------------------------------------
checkTerm a wh _ctx m@(MRef (MRPrm nPrim)) Synth
 | Just pp <- Map.lookup nPrim Prim.primOps
 = do   let tPrim = mapAnnot (const a) $ Prim.typeOfPrim pp
        return (m, [tPrim])

 | Just t  <- Map.lookup nPrim Prim.primDataCtors
 = do   let tPrim = mapAnnot (const a) t
        return (m, [tPrim])

 | otherwise
 = throw $ ErrorUnknownPrimitive a wh nPrim


-- (t-con) ------------------------------------------------
checkTerm a wh ctx m@(MRef (MRCon nCon)) Synth
 =  contextResolveDataCtor nCon ctx
 >>= \case
        Nothing
         -> throw $ ErrorUnknownDataCtor a wh nCon

        Just tCtor
         -> do  let tCtor' = mapAnnot (const a) tCtor
                return (m, [tCtor'])


-- (t-var) ------------------------------------------------
checkTerm a wh ctx m@(MVar u) Synth
 = do   mt <- contextResolveTermBound u ctx
        case mt of
         Just t  -> return (m, [t])
         Nothing -> do error (show ctx); throw $ ErrorUnknownTermBound a wh u


-- (t-abt) ------------------------------------------------
checkTerm a wh ctx (MAbs ps@MPTypes{} m) Synth
 = do   ps'@(MPTypes bts) <- checkTermParams a wh ctx ps
        let ctx' =  contextBindTermParams ps ctx
        (m', t)  <- checkTerm1 a wh ctx' m Synth
        return  (MAbs ps' m', [TForall bts t])


-- (t-abm) ------------------------------------------------
checkTerm a wh ctx (MAbs ps@MPTerms{} m) Synth
 = do   ps'@(MPTerms bts) <- checkTermParams a wh ctx ps
        let ctx' =  contextBindTermParams ps ctx
        (m', ts) <- checkTerm a wh ctx' m Synth
        return  (MAbs ps' m', [TFun (map snd bts) ts])


-- (t-apt) ------------------------------------------------
checkTerm a wh ctx (MApp mFun (MGTypes tsArg)) Synth
 = do   (mFun',  tFun)     <- checkTerm1 a wh ctx mFun Synth
        (tsArg', tResult)  <- checkTermAppTypes a wh ctx tFun tsArg
        return (MApp mFun' (MGTypes tsArg'), [tResult])


-- (t-apm) ------------------------------------------------
checkTerm a wh ctx (MApp mFun (MGTerms msArg)) Synth
 = do   (mFun',  tFun)     <- checkTerm1 a wh ctx mFun Synth
        (msArg', tsResult) <- checkTermAppTerms a wh ctx tFun msArg
        return (MApp mFun' (MGTerms msArg'), tsResult)


-- (t-apv) ------------------------------------------------
checkTerm a wh ctx (MApp mFun (MGTerm mArg)) Synth
 = do   (mFun', tFun)      <- checkTerm1 a wh ctx mFun Synth
        (mArg', tsResult)  <- checkTermAppTerm a wh ctx tFun mArg
        return (MApp mFun' (MGTerm mArg'),   tsResult)


-- (t-let) ------------------------------------------------
checkTerm a wh ctx (MLet bts mBind mResult) Synth
 = do   MPTerms bts'      <- checkTermParams a wh ctx (MPTerms bts)
        (mBind', tsBind)  <- checkTerm a wh ctx mBind Synth

        let checkLetAnnot tAnnot tBind
             | THole    <- tAnnot
             = return tBind

             | otherwise
             = case checkTypeEq a [] tAnnot a [] tBind of
                 Nothing        -> return tBind
                 Just ((_a1, tErr1), (_a2, tErr2))
                  -> throw $ ErrorTypeMismatch a wh tErr1 tErr2

        -- TODO: check num. of bindings
        let (bs, tsParam) = unzip bts'
        when (not $ length tsParam == length tsBind)
         $ throw $ ErrorLetWrongArity a wh tsBind bs

        tsBind'   <- zipWithM checkLetAnnot tsParam tsBind
        let bts'' =  zip bs tsBind'

        let ctx' = contextBindTermParams (MPTerms bts'') ctx
        (mResult', tsResult) <- checkTerm a wh ctx' mResult Synth
        return  (MLet bts' mBind' mResult', tsResult)


-- (t-rec) ------------------------------------------------
-- TODO: check for repeated labels.
checkTerm a wh ctx mm@(MRecord ns ms) mode
 = case mode of
        Check [TRecord ns' tsExpected]
         | Set.fromList ns == Set.fromList ns'
         , Set.size (Set.fromList ns)  == length ns
         , Set.size (Set.fromList ns') == length ns'
         -> do
                let nts = Map.fromList $ zip ns' tsExpected
                let nmts = [ let Just t = Map.lookup n nts in (n, m, t)
                           | m  <- ms | n <- ns ]

                (ms', ts')
                  <- fmap unzip $ forM nmts $ \(n, m, t)
                  -> do let wh' = WhereRecordField a n (Just t) : wh
                        checkTerm1 a wh' ctx m (Check [t])

                return (MRecord ns ms', [TRecord ns ts'])

        Check _
         -> checkTerm_default a wh ctx mm mode

        Synth
         -> do  (ms', ts') <- checkTerms a wh ctx ms Synth
                return  (MRecord ns ms', [TRecord ns ts'])


-- (t-prj) ------------------------------------------------
checkTerm a wh ctx (MProject nLabel mRecord) Synth
 = do   (mRecord', tsRecord) <- checkTerm a wh ctx mRecord Synth
        case tsRecord of
         [tRecord@(TRecord ns ts)]
          -> case lookup nLabel $ zip ns ts of
                Nothing     -> throw $ ErrorRecordProjectNoField a wh tRecord nLabel
                Just tField -> return (MProject nLabel mRecord', [tField])

         [tThing]
            -> throw $ ErrorRecordProjectIsNot a wh tThing nLabel
         ts -> throw $ ErrorTermsWrongArity a wh ts [TData]


-- (t-vnt) ------------------------------------------------
checkTerm a wh ctx (MVariant nLabel mValue tResult) Synth
 = do   (mValue', _tValue) <- checkTerm1 a wh ctx mValue Synth

        -- TODO: check variant is contained in given type
        -- TODO: check given type is a variant type.
        return  (MVariant nLabel mValue' tResult, [tResult])


-- (t-if) -------------------------------------------------
checkTerm a wh ctx (MIf msCond msThen mElse) Synth
 = do   (msCond', _tssCond) <- checkTerms a wh ctx msCond
                            $  Check $ replicate (length msCond) TBool
        (msThen', _tssThen) <- checkTerms a wh ctx msThen Synth
        (mElse',  tsElse)   <- checkTerm  a wh ctx mElse  Synth

        -- TODO: check tsThen and tsElse matches
        -- TODO: check there are the same number of conds and then exps.
        return  (MIf msCond' msThen' mElse', tsElse)


-- (t-lst) ------------------------------------------------
-- TODO: check embedded type.
checkTerm a wh ctx (MList t ms) Synth
 = do   let ts = replicate (length ms) t
        (ms', _) <- checkTerms a wh ctx ms (Check ts)
        return  (MList t ms', [TList t])


-- (t-set) ------------------------------------------------
-- TODO: check embedded type.
checkTerm a wh ctx (MSet t ms) Synth
 = do   let ts = replicate (length ms) t
        (ms', _) <- checkTerms a wh ctx ms (Check ts)
        return (MSet t ms', [TSet t])


-- (t-map) ------------------------------------------------
-- TODO: check embedded types.
-- TODO: check keys and values same length.
checkTerm a wh ctx (MMap tk tv msk msv) Synth
 = do   let tsk = replicate (length msk) tk
        let tsv = replicate (length msv) tv
        (msk', _) <- checkTerms a wh ctx msk (Check tsk)
        (msv', _) <- checkTerms a wh ctx msv (Check tsv)
        return (MMap tk tv msk' msv', [TMap tk tv])


checkTerm a wh ctx m mode
 = checkTerm_default a wh ctx m mode


-----------------------------------------------------------
checkTerm_default a wh ctx m (Check tsExpected)
 = do   (m', tsActual) <- checkTerm a wh ctx m Synth

        when (length tsActual /= length tsExpected)
         $ throw $ ErrorTermsWrongArity a wh tsActual tsExpected

        case checkTypeEqs a [] tsExpected a [] tsActual of
         Nothing -> return (m', tsActual)
         Just ((_a1, t1Err), (_a2, t2Err))
          -> throw $ ErrorTypeMismatch a wh t1Err t2Err


checkTerm_default _ _ _ mm mode
 =  error $ unlines
        [ "checkTerm: not finished"
        , ppShow mode
        , ppShow mm
        , P.renderIndent $ P.ppr () mm ]



-- (t-one) ----------------------------------------------------------------------------------------
-- | Like 'checkTerm' but expect a single result type.
checkTerm1
        :: Annot a => a -> [Where a]
        -> Context a -> Term a -> Mode a -> IO (Term a, Type a)
checkTerm1 a wh ctx m mode
 = do   (m', ts') <- checkTerm a wh ctx m mode
        case ts' of
         [t]    -> return (m', t)
         _      -> throw $ ErrorTermsWrongArity a wh ts' [TData]


-- | Like 'checkTerm', but expect the given result result type,
--   throwing an error if it's not.
checkTermIs1
        :: Annot a => a -> [Where a]
        -> Context a -> Term a -> Type a -> IO (Term a)
checkTermIs1 a wh ctx m tExpected
 = do   (m', _t') <- checkTerm1 a wh ctx m (Check [tExpected])

        -- TODO: type equality
        return m'


-- (t-many / t-gets) ------------------------------------------------------------------------------
-- This function implements both the t-many and t-gets rules from the declarative
-- version of the typing rules. In this algorithmic, bidirectional implementation
-- the expected types are represented in the checker mode we get from the caller.

-- | Check a list of individual terms.
checkTerms
        :: Annot a => a -> [Where a]
        -> Context a -> [Term a] -> Mode a -> IO ([Term a], [Type a])

checkTerms a wh ctx ms Synth
 =      fmap unzip $ mapM     (\m -> checkTerm1 a wh ctx m Synth) ms

checkTerms a wh ctx ms (Check ts)
 | length ms == length ts
 =      fmap unzip $ zipWithM (\m t -> checkTerm1 a wh ctx m (Check [t])) ms ts

 | otherwise
 = do   (_ms, ts') <- fmap unzip $ mapM (\m -> checkTerm1 a wh ctx m Synth) ms
        throw $ ErrorTermsWrongArity a wh ts' (replicate (length ms) TData)


---------------------------------------------------------------------------------------------------
checkTermParams
        :: Annot a => a -> [Where a]
        -> Context a -> TermParams a -> IO (TermParams a)

checkTermParams _a _wh _ctx tps
 = case tps of
        MPTerms bts -> return $ MPTerms bts   -- TODO: check types
        MPTypes bts -> return $ MPTypes bts   -- TODO: check types


---------------------------------------------------------------------------------------------------
-- | Check the application of a term to some types.
checkTermAppTypes
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Type a] -> IO ([Type a], Type a)

checkTermAppTypes a wh ctx tFun tsArg
 = case tFun of
        TForall bksParam tResult
          -> goCheckArgs bksParam tResult
        _ -> throw $ ErrorAppTermTypeCannot a wh tFun

 where  -- TODO: kind check parameter types.

        goCheckArgs  bksParam tResult
         = do   (tsArg', ksArg) <- checkTypes a wh ctx tsArg
                if length bksParam /= length tsArg
                 then throw $ ErrorAppTermTypeWrongArity a wh bksParam tsArg
                 else goCheckParams bksParam tResult tsArg' ksArg

        goCheckParams bksParam tResult tsArg' ksArg
         = case checkTypeEqs a [] (map snd bksParam) a [] ksArg of
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                 -> throw $ ErrorTypeMismatch a wh tErr1 tErr2

                Nothing -> goSubstArgs bksParam tResult tsArg'

        goSubstArgs   bksParam tResult tsArg'
         = do   let subst   = Map.fromList
                                [ (n, t) | (BindName n, _k) <- bksParam
                                         | t <- tsArg ]
                let tSubst  = substTypeType [subst] tResult
                return  (tsArg', tSubst)


-- | Check the application of a functional term to an argument term.
--   Reduction of the argument may produce a vector of values.
checkTermAppTerm
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> Term a -> IO (Term a, [Type a])

checkTermAppTerm a wh ctx tFun mArg
 = do   let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot a wh tFun)
                $ takeTFun tFun

        (mArg', tsArg)
         <- checkTerm a wh ctx mArg (Check tsParam)

        when (not $ length tsParam == length tsArg)
         $ throw $ ErrorAppTermTermWrongArity a wh tsParam tsArg

        case checkTypeEqs a [] tsParam a [] tsArg of
         Just ((_aErr1', tErr1), (_aErr2', tErr2))
           -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
         _ -> return (mArg', tsResult)


-- | Check the application of a functional term to some argument terms.
--   The arguments can only produce a single value each.
checkTermAppTerms
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Term a] -> IO ([Term a], [Type a])

checkTermAppTerms a wh ctx tFun msArg
 = do   let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot a wh tFun)
                $ takeTFun tFun

        when (not $ length msArg == length tsParam)
         $ throw $ ErrorAppTermTermWrongArityNum a wh tsParam (length msArg)

        (msArg', tsArg)
         <- checkTerms a wh ctx msArg (Check tsParam)

        case checkTypeEqs a [] tsParam a [] tsArg of
         Just ((_aErr1', tErr1), (_aErr2', tErr2))
           -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
         _ -> return  (msArg', tsResult)

