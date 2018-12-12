
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
        -> Term a -> Mode a
        -> IO (Term a, [Type a], [Effect a])

-- (t-ann) ------------------------------------------------
checkTerm _a wh ctx (MAnn a' m) mode
 = checkTerm a' wh ctx m mode


-- (t-mmm) ------------------------------------------------
checkTerm a wh ctx (MTerms msArg) mode
 = do   (msArg', tsArg, esArg)
         <- checkTerms a wh ctx msArg mode
        return  (MTerms msArg', tsArg, esArg)


-- (t-the) ------------------------------------------------
checkTerm a wh ctx (MThe ts m) Synth
 = do   (m', _, es) <- checkTerm a wh ctx m (Check ts)
        return  (MThe ts m', ts, es)


-- (t-box) ------------------------------------------------
checkTerm a wh ctx (MBox m) Synth
 = do   (m', ts, es) <- checkTerm a wh ctx m Synth
        -- TODO: crush effects before boxing them.
        return  (MBox m', [TSusp es ts], [])


-- (t-run) ------------------------------------------------
checkTerm a wh ctx (MRun m) Synth
 = do   (m', ts, es) <- checkTerm a wh ctx m Synth

        -- TODO: look through any annots.
        case ts of
         [TSusp es' ts'] -> return (MRun m', ts', es ++ es')
         _               -> error $ "not a suspension" ++ show ts


-- (t-val) ------------------------------------------------
checkTerm _a _wh _ctx m@(MRef (MRVal v)) Synth
 = case v of
        VUnit     -> return (m, [TUnit],     [])
        VSymbol{} -> return (m, [TSymbol],   [])
        VText{}   -> return (m, [TText],     [])
        VBool{}   -> return (m, [TBool],     [])
        VInt{}    -> return (m, [TInt],      [])
        VNat{}    -> return (m, [TNat],      [])
        (VNone t) -> return (m, [TOption t], [])
        _         -> error "check value not done yet"


-- (t-prm) ------------------------------------------------
checkTerm a wh _ctx m@(MRef (MRPrm nPrim)) Synth
 | Just pp <- Map.lookup nPrim Prim.primOps
 = do   let tPrim = mapAnnot (const a) $ Prim.typeOfPrim pp
        return (m, [tPrim], [])

 | Just t  <- Map.lookup nPrim Prim.primDataCtors
 = do   let tPrim = mapAnnot (const a) t
        return (m, [tPrim], [])

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
                return (m, [tCtor'], [])


-- (t-var) ------------------------------------------------
checkTerm a wh ctx m@(MVar u) Synth
 = do   mt <- contextResolveTermBound u ctx
        case mt of
         Just t  -> return (m, [t], [])
         Nothing -> throw $ ErrorUnknownTermBound a wh u


-- (t-abt) ------------------------------------------------
checkTerm a wh ctx (MAbs ps@MPTypes{} m) Synth
 = do   ps'@(MPTypes bts) <- checkTermParams a wh ctx ps
        let ctx' =  contextBindTermParams ps ctx
        (m', t, _es)  <- checkTerm1 a wh ctx' m Synth
        -- TODO: check effects are empty.
        return  (MAbs ps' m', [TForall bts t], [])


-- (t-abm) ------------------------------------------------
checkTerm a wh ctx (MAbs ps@MPTerms{} m) Synth
 = do   ps'@(MPTerms bts) <- checkTermParams a wh ctx ps
        let ctx' =  contextBindTermParams ps ctx
        (m', ts, _es) <- checkTerm a wh ctx' m Synth
        -- TODO: check effects are empty.
        return  (MAbs ps' m', [TFun (map snd bts) ts], [])


-- (t-aps) ------------------------------------------------
-- This handles (t-apt), (t-apm) and (t-apv) from the docs.
-- TODO: redo typing rules to use spine form for argument lists.
checkTerm a wh ctx (MAps mFun0 mgss0) Synth
 = do
        (mFun1, tFun1, esFun)
         <- checkTerm1 a wh ctx mFun0 Synth

        let -- (t-apt) -----
            checkApp [tFun] es (MGTypes tsArg : mgss) mgssAcc
             = do (tsArg', tResult)  <- checkTermAppTypes a wh ctx tFun tsArg
                  checkApp [tResult] es mgss (MGTypes tsArg' : mgssAcc)

            -- (t-apm) -----
            checkApp [tFun] es (MGTerms msArg : mgss) mgssAcc
             = do (msArg', tsResult, es') <- checkTermAppTerms a wh ctx tFun msArg
                  checkApp tsResult (es ++ es') mgss (MGTerms msArg' : mgssAcc)

            -- (t-apv) -----
            checkApp [tFun] es (MGTerm  mArg  : mgss) mgssAcc
             = do (mArg',  tsResult, es') <- checkTermAppTerm  a wh ctx tFun mArg
                  checkApp tsResult (es ++ es') mgss (MGTerm  mArg'  : mgssAcc)

            checkApp tsResult es [] mgssAcc
             = return (MAps mFun1 (reverse mgssAcc), tsResult, es)

            checkApp _ _ _ _
             = error "arity error"

        checkApp [tFun1] esFun mgss0 []


-- (t-let) ------------------------------------------------
checkTerm a wh ctx (MLet bts mBind mResult) Synth
 = do   MPTerms bts' <- checkTermParams a wh ctx (MPTerms bts)
        (mBind', tsBind, esBind) <- checkTerm a wh ctx mBind Synth

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
        (mResult', tsResult, esResult) <- checkTerm a wh ctx' mResult Synth
        return  ( MLet bts' mBind' mResult'
                , tsResult
                , esBind ++ esResult)


-- (t-rec) ------------------------------------------------
-- TODO: check for repeated labels.
checkTerm a wh ctx mm@(MRecord ns ms) mode
 = case mode of
        Check [TRecord ns' tgsExpected]
         | Set.fromList ns == Set.fromList ns'
         , Set.size (Set.fromList ns)  == length ns
         , Set.size (Set.fromList ns') == length ns'
         -> do
                let nts   = Map.fromList $ zip ns' tgsExpected
                let nmtgs = [ let Just tg = Map.lookup n nts in (n, m, tg)
                            | m  <- ms | n <- ns ]

                (ms', tss', ess')
                  <- fmap unzip3 $ forM nmtgs $ \(n, m, (TGTypes ts))
                  -> do let wh' = WhereRecordField a n (Just ts) : wh
                        checkTerm a wh' ctx m (Check ts)

                return  ( MRecord ns ms'
                        , [TRecord ns $ map TGTypes tss']
                        , concat ess')

        Check _
         -> checkTerm_default a wh ctx mm mode

        Synth
         -> do  (ms', tss', ess')
                 <- fmap unzip3
                  $ mapM (\m -> checkTerm a wh ctx m Synth) ms

                return  ( MRecord ns ms'
                        , [TRecord ns (map TGTypes tss')]
                        , concat ess')


-- (t-prj) ------------------------------------------------
checkTerm a wh ctx (MProject nLabel mRecord) Synth
 = do   (mRecord', tsRecord, esRecord)
         <- checkTerm a wh ctx mRecord Synth

        case tsRecord of
         [tRecord@(TRecord ns tgs)]
          -> case lookup nLabel $ zip ns tgs of
                Nothing
                 -> throw $ ErrorRecordProjectNoField a wh tRecord nLabel

                Just (TGTypes tsField)
                 -> return (MProject nLabel mRecord', tsField, esRecord)

         [tThing]
            -> throw $ ErrorRecordProjectIsNot a wh tThing nLabel
         ts -> throw $ ErrorTermsWrongArity a wh ts [TData]


-- (t-vnt) ------------------------------------------------
checkTerm a wh ctx (MVariant nLabel mValues tResult) Synth
 = do   (mValues', _tsValues, esValues) <- checkTerm a wh ctx mValues Synth

        -- TODO: check variant is contained in given type
        -- TODO: check given type is a variant type.
        return  ( MVariant nLabel mValues' tResult
                , [tResult], esValues)


-- (t-cse) ------------------------------------------------
checkTerm a wh ctx (MCase mScrut ls msAlt) Synth
 = do   (mScrut', _tScrut, esScrut)
         <- checkTerm1 a wh ctx mScrut Synth

        (msAlt',  tsAlt, esAltComp)
         <- fmap unzip3 $ mapM (\m -> checkTerm1 a wh ctx m Synth) msAlt

        -- TODO: check alts all have same result type.
        -- TODO: check scrut matches alt head.
        -- TODO: combine result effects of all alts.
        let (TFun _ tsResult : _) = tsAlt
        return  ( MCase mScrut' ls msAlt'
                , tsResult
                , esScrut ++ concat esAltComp)


-- (t-ifs) ------------------------------------------------
checkTerm a wh ctx (MIf msCond msThen mElse) Synth
 = do   (msCond', _tssCond, esCond)
         <- checkTerms a wh ctx msCond
         $  Check $ replicate (length msCond) TBool

        (msThen', _tssThen, esThen)
         <- checkTerms a wh ctx msThen Synth

        (mElse',  tsElse, esElse)
         <- checkTerm  a wh ctx mElse  Synth

        -- TODO: check tsThen and tsElse matches
        -- TODO: check there are the same number of conds and then exps.
        return  ( MIf msCond' msThen' mElse'
                , tsElse
                , esCond ++ esThen ++ esElse)


-- (t-lst) ------------------------------------------------
-- TODO: check embedded type.
checkTerm a wh ctx (MList t ms) Synth
 = do   let ts = replicate (length ms) t
        (ms', _, es) <- checkTerms a wh ctx ms (Check ts)
        return  (MList t ms', [TList t], es)


-- (t-set) ------------------------------------------------
-- TODO: check embedded type.
checkTerm a wh ctx (MSet t ms) Synth
 = do   let ts = replicate (length ms) t
        (ms', _, es) <- checkTerms a wh ctx ms (Check ts)
        return (MSet t ms', [TSet t], es)


-- (t-map) ------------------------------------------------
-- TODO: check embedded types.
-- TODO: check keys and values same length.
checkTerm a wh ctx (MMap tk tv msk msv) Synth
 = do   let tsk = replicate (length msk) tk
        let tsv = replicate (length msv) tv
        (msk', _, esKeys) <- checkTerms a wh ctx msk (Check tsk)
        (msv', _, esVals) <- checkTerms a wh ctx msv (Check tsv)
        return  ( MMap tk tv msk' msv'
                , [TMap tk tv]
                , esKeys ++ esVals)

checkTerm a wh ctx m mode
 = checkTerm_default a wh ctx m mode


-----------------------------------------------------------
checkTerm_default a wh ctx m (Check tsExpected)
 = do   (m', tsActual, esActual)
         <- checkTerm a wh ctx m Synth

        when (length tsActual /= length tsExpected)
         $ throw $ ErrorTermsWrongArity a wh tsActual tsExpected

        case checkTypeEqs a [] tsExpected a [] tsActual of
         Nothing -> return (m', tsActual, esActual)
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
        -> Context a -> Term a -> Mode a
        -> IO (Term a, Type a, [Effect a])

checkTerm1 a wh ctx m mode
 = do   (m', ts', es')
         <- checkTerm a wh ctx m mode

        case ts' of
         [t]    -> return (m', t, es')
         _      -> throw $ ErrorTermsWrongArity a wh ts' [TData]


-- | Like 'checkTerm', but expect the given result result type,
--   throwing an error if it's not.
checkTermIs1
        :: Annot a => a -> [Where a]
        -> Context a -> Term a -> Type a
        -> IO (Term a, [Effect a])

checkTermIs1 a wh ctx m tExpected
 = do   (m', _t', es')
         <- checkTerm1 a wh ctx m (Check [tExpected])

        -- TODO: type equality
        return (m', es')


-- (t-many / t-gets) ------------------------------------------------------------------------------
-- This function implements both the t-many and t-gets rules from the declarative
-- version of the typing rules. In this algorithmic, bidirectional implementation
-- the expected types are represented in the checker mode we get from the caller.

-- | Check a list of individual terms.
checkTerms
        :: Annot a => a -> [Where a]
        -> Context a -> [Term a] -> Mode a
        -> IO ([Term a], [Type a], [Effect a])

checkTerms a wh ctx ms Synth
 = do   (ms', ts', ess')
         <- fmap unzip3 $ mapM     (\m -> checkTerm1 a wh ctx m Synth) ms
        return (ms', ts', concat ess')

checkTerms a wh ctx ms (Check ts)
 | length ms == length ts
 = do   (ms', ts', ess')
         <- fmap unzip3 $ zipWithM (\m t -> checkTerm1 a wh ctx m (Check [t])) ms ts
        return (ms', ts', concat ess')

 | otherwise
 = do   (_ms, ts', _ess')
         <- fmap unzip3 $ mapM (\m -> checkTerm1 a wh ctx m Synth) ms
        throw $ ErrorTermsWrongArity a wh ts' (replicate (length ms) TData)


---------------------------------------------------------------------------------------------------
checkTermParams
        :: Annot a => a -> [Where a]
        -> Context a -> TermParams a
        -> IO (TermParams a)

checkTermParams _a _wh _ctx tps
 = case tps of
        MPTerms bts -> return $ MPTerms bts   -- TODO: check types
        MPTypes bts -> return $ MPTypes bts   -- TODO: check types


---------------------------------------------------------------------------------------------------
-- | Check the application of a term to some types.
checkTermAppTypes
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Type a]
        -> IO ([Type a], Type a)

checkTermAppTypes a wh ctx tFun tsArg
 = do
        -- The funtion needs to have a forall type.
        (bksParam, tResult)
         <- case tFun of
                TForall bksParam tResult
                  -> return (bksParam, tResult)
                _ -> throw $ ErrorAppTermTypeCannot a wh tFun

        -- Check the kinds of the arguments.
        (tsArg', ksArg) <- checkTypes a wh ctx tsArg

        -- The number of arguments must match the number of parameters.
        when (not $ length bksParam == length tsArg)
         $ throw $ ErrorAppTermTypeWrongArity a wh bksParam tsArg

        -- Check the parameter and argument kinds match.
        (case checkTypeEqs a [] (map snd bksParam) a [] ksArg of
                Just ((_aErr1', tErr1), (_aErr2', tErr2))
                  -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
                _ -> return ())

        -- Substitute arguments into the result type to instantiate
        -- the type scheme.
        let subst   = Map.fromList
                        [ (n, t) | (BindName n, _k) <- bksParam
                                 | t <- tsArg ]
        let tSubst  = substTypeType [subst] tResult

        -- Return the checked argument types and the instantiated scheme.
        return  (tsArg', tSubst)


-- | Check the application of a functional term to an argument term.
--   Reduction of the argument may produce a vector of values.
checkTermAppTerm
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> Term a
        -> IO (Term a, [Type a], [Effect a])

checkTermAppTerm a wh ctx tFun mArg
 = do
        -- The function needs to have a functional type.
        let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot a wh tFun)
                $ takeTFun tFun

        -- Check the types of the argument.
        (mArg', tsArg, esArg)
         <- checkTerm a wh ctx mArg (Check tsParam)

        -- The number of arguments must match the number of parameters.
        when (not $ length tsParam == length tsArg)
         $ throw $ ErrorAppTermTermWrongArity a wh tsParam tsArg

        -- Check the parameter and argument tpyes match.
        case checkTypeEqs a [] tsParam a [] tsArg of
         Just ((_aErr1', tErr1), (_aErr2', tErr2))
           -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
         _ -> return (mArg', tsResult, esArg)


-- | Check the application of a functional term to some argument terms.
--   The arguments can only produce a single value each.
checkTermAppTerms
        :: Annot a => a -> [Where a]
        -> Context a -> Type a -> [Term a]
        -> IO ([Term a], [Type a], [Effect a])

checkTermAppTerms a wh ctx tFun msArg
 = do
        -- The function needs to have a functional type.
        let (tsParam, tsResult)
                = fromMaybe (throw $ ErrorAppTermTermCannot a wh tFun)
                $ takeTFun tFun

        -- The number of arguments must match the number of parameters.
        when (not $ length msArg == length tsParam)
         $ throw $ ErrorAppTermTermWrongArityNum a wh tsParam (length msArg)

        -- Check the types of the arguments.
        (msArg', tsArg, esArgs)
         <- checkTerms a wh ctx msArg (Check tsParam)

        -- Check the parameter and argument types match.
        case checkTypeEqs a [] tsParam a [] tsArg of
         Just ((_aErr1', tErr1), (_aErr2', tErr2))
           -> throw $ ErrorTypeMismatch a wh tErr1 tErr2
         _ -> return  (msArg', tsResult, esArgs)

