
module Salt.Core.Check.Term.Value where
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Check a value, yielding its type.
synthValue
        :: Annot a => a -> [Where a]
        -> Context a -> Value a -> IO (Type a)

synthValue a wh ctx v
 = case v of
        VUnit      -> return TUnit
        VSymbol{}  -> return TSymbol
        VText{}    -> return TText
        VBool{}    -> return TBool
        VNat{}     -> return TNat
        VInt{}     -> return TInt
        VWord{}    -> return TWord
        VInt8{}    -> return TInt8
        VInt16{}   -> return TInt16
        VInt32{}   -> return TInt32
        VInt64{}   -> return TInt64
        VWord8{}   -> return TWord8
        VWord16{}  -> return TWord16
        VWord32{}  -> return TWord32
        VWord64{}  -> return TWord64
        VNone t    -> return $ TOption t

        VLoc t _i
         -> do  checkType a wh ctx t
                return $ TCell t

        VAddr _    -> return TAddr
        VPtr r t _ -> return $ TPtr r t

        VData n ts vs
         -> do  -- Use the term checker to check the applications.
                (_m, tResult, [])
                 <- synthTermProductive1 a wh ctx
                        (MApm (MApt (MCon n) ts) (map MVal vs))
                return tResult

        VRecord nvs
         -> do  let (ns, vss) = unzip nvs
                tss <- mapM (mapM (synthValue a wh ctx)) vss
                return (TRecord ns $ map TGTypes tss)

        VVariant _n tVar vs
         -> do  checkType a wh ctx tVar
                mapM_ (synthValue a wh ctx) vs
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

        VExtPair val types asc
         -> do  mapM (checkType a wh ctx) types
                checkType a wh ctx asc
                -- TODO FIXME maybe also check that the type for the term is
                -- equivalent to the type given as the ascription.
                -- see syncTermWith for MPack for details.
                synthValue a wh ctx val
                return asc

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
                 <- synthTermProductive a wh ctx2 mBody

                -- The body must be pure.
                eBody_red <- simplType a ctx2 (TSum esBody)
                when (not $ isTPure eBody_red)
                 $ throw $ ErrorAbsImpure UTerm a wh eBody_red

                return $ TFun tsParam tsResult

        -- TODO: we should check the components of the bundle similarly
        -- to how whole modules are checked. To achieve this the decls
        -- need their kind and type sigs attached.
        VBundle _
         -> do  return $ TBundle

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
 = do   tActual <- synthValue a wh ctx v
        checkTypeEquivs ctx a [] [tExpected] a [] [tActual]
         >>= \case
                Nothing -> return ()
                Just ((_a1, t1Err), (_a2, t2Err))
                 -> throw $ ErrorMismatch UType a wh t1Err t2Err


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
                ts' <- fmap (map (\(_, t, _) -> t))
                    $  mapM (\v -> synthTermProductive1 a wh ctx (MVal v)) vs
                let nts' = zip ns ts'
                go (contextBindTerms nts' ctx) bs

        go _ctx (TermEnvValuesRec _ncs : _bts)
         = error "TODO: check recursive term envs"

        go ctx []
         = return $ ctx

