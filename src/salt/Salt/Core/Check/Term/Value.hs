
module Salt.Core.Check.Term.Value where
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Check a value, yielding its type.
checkValue
        :: Annot a => a -> [Where a]
        -> Context a -> Value a -> IO (Type a)

checkValue a wh ctx v
 = case v of
        VUnit     -> return TUnit
        VSymbol{} -> return TSymbol
        VText{}   -> return TText
        VBool{}   -> return TBool
        VNat{}    -> return TNat
        VInt{}    -> return TInt
        VInt8{}   -> return TInt8
        VInt16{}  -> return TInt16
        VInt32{}  -> return TInt32
        VInt64{}  -> return TInt64
        VWord{}   -> return TWord
        VWord8{}  -> return TWord8
        VWord16{} -> return TWord16
        VWord32{} -> return TWord32
        VWord64{} -> return TWord64
        VNone t   -> return $ TOption t

        VData n ts vs
         -> do  -- Use the term checker to check the applications.
                (_m, tResult, [])
                 <- checkTerm1 a wh ctx Synth
                        (MApm (MApt (MCon n) ts) (map MVal vs))
                return tResult

        VRecord nvs
         -> do  let (ns, vss) = unzip nvs
                tss <- mapM (mapM (checkValue a wh ctx)) vss
                return (TRecord ns $ map TGTypes tss)

        VVariant _n tVar vs
         -> do  checkType a wh ctx tVar
                mapM_ (checkValue a wh ctx) vs
                return tVar

        VList t vs
         -> do  checkType a wh ctx t
                checkValuesAreAll a wh ctx t vs
                return $ TList t

        VSet t vsSet
         -> do  checkType a wh ctx t
                checkValuesAreAll a wh ctx t  $ map (mapAnnot (const a)) $ Set.toList vsSet
                return $ TSet t

        VMap tk tv vsMap
         -> do  checkType a wh ctx tk
                checkType a wh ctx tv
                checkValuesAreAll a wh ctx tk $ map (mapAnnot (const a)) $ Map.keys vsMap
                checkValuesAreAll a wh ctx tv $ Map.elems vsMap
                return $ TMap tk tv

        VClosure (TermClosure env mps mBody)
         -> do  -- Build a context with just the closure environment.
                ctx1    <- contextBindTermEnv a wh env
                        $  ctx
                        {  contextModuleTerm = contextModuleTerm ctx
                        ,  contextLocal      = []}

                -- Bind the closure parameters into the context.
                MPTerms btsParam <- checkTermParams a wh ctx mps
                let tsParam     = map snd btsParam
                let ctx2        = contextBindTermParams mps ctx1

                -- Check the body expression.
                (_, tsResult, esBody)
                 <- checkTerm a wh ctx2 Synth mBody

                -- The body must be pure.
                eBody_red <- simplType a ctx2 (TSum esBody)
                when (not $ isTPure eBody_red)
                 $ throw $ ErrorAbsTermImpure a wh eBody_red

                return $ TFun tsParam tsResult


-- | Check that a list of values all have the given type.
checkValuesAreAll
        :: Annot a => a -> [Where a] -> Context a
        -> Type a -> [Value a] -> IO ()

checkValuesAreAll a wh ctx t vs
 = mapM_ (checkValueIs a wh ctx t) vs


-- | Check that a value has the given type.
checkValueIs
        :: Annot a => a -> [Where a] -> Context a
        -> Type a -> Value a -> IO ()

checkValueIs a wh ctx tExpected v
 = do   tActual <- checkValue a wh ctx v

        checkTypeEquivs ctx a [] [tExpected] a [] [tActual]
         >>= \case
                Nothing -> return ()
                Just ((_a1, t1Err), (_a2, t2Err))
                 -> throw $ ErrorTypeMismatch a wh t1Err t2Err


-- | Check an environment binding and add it to the context.
contextBindTermEnv
        :: Annot a
        => a -> [Where a] -> TermEnv a
        -> Context a -> IO (Context a)

contextBindTermEnv a wh (TermEnv bs0) ctx0
 = go ctx0 (reverse bs0)
 where
        go ctx (TermEnvTypes nts : bs)
         = do   let (ns, ts) = unzip $ Map.toList nts
                ks'     <- fmap (map snd)
                        $  mapM (checkType a wh ctx) ts
                let nks' = zip ns ks'
                go (contextBindTypes nks' ctx) bs

        go ctx (TermEnvValues nvs : bs)
         = do   let (ns, vs) = unzip $ Map.toList nvs
                ts'     <- fmap (map (\(_, t, _) -> t))
                        $  mapM (\v -> checkTerm1 a wh ctx Synth (MVal v)) vs
                let nts' = zip ns ts'
                go (contextBindTerms nts' ctx) bs

        go ctx []
         = return $ ctx

