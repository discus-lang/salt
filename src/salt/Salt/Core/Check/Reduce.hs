
module Salt.Core.Check.Reduce where
import Salt.Core.Check.Where
import Salt.Core.Check.Context
import Salt.Core.Transform.Snv
import Salt.Core.Exp


---------------------------------------------------------------------------------------------------
-- | Reduce a type to weak head normal form.
--   If the head is also an effect type then normalize it.
reduceType
        :: Annot a => a -> [Where a] -> Context a
        -> Type a -> IO (Maybe (Type a))

reduceType a wh ctx (TAnn _a t)
 = reduceType a wh ctx t

reduceType a wh ctx tt@(TVar u)
 = contextResolveTypeBound u ctx
 >>= \case
       Just (_k, Just tSyn)
         ->  reduceType a wh ctx tSyn
         >>= \case
               Just tRed  -> return $ Just tRed
               Nothing    -> return $ Just tSyn

       _ -> return $ Just tt

-- TODO: flattenType should report whether it's done anything.
reduceType _a _wh _ctx tt@(TSum{})
 = return $ Just $ flattenType tt

reduceType a wh ctx (TApt tFun tsArgs)
 = simplType a wh ctx tFun
 >>= \case
        TAbs (TPTypes bks) tBody
         | length bks == length tsArgs
         -> do  let ns     = [n | BindName n <- map fst bks]
                let snv    = snvOfBinds $ zip ns tsArgs
                let tBody' = snvApplyType upsEmpty snv tBody
                reduceType a wh ctx tBody'
                 >>= \case
                        Just tRed -> return $ Just tRed
                        Nothing   -> return $ Just tBody'

        _ -> return Nothing

reduceType _ _ _ _
 =      return Nothing


---------------------------------------------------------------------------------------------------
-- | Like `reduceType` but just return the original type if it cannot be
--   reduced further.
simplType
        :: Annot a => a -> [Where a] -> Context a
        -> Type a -> IO (Type a)

simplType a wh ctx tt
 = reduceType a wh ctx tt
 >>= \case
        Nothing  -> return tt
        Just tt' -> return tt'


-- | Simplify all the given types to head normal form.
--   We look through annotations, unfold synonyms and reduce type applications
--   to reveal the head constructor.
simplTypes
        :: Annot a => a -> [Where a] -> Context a
        -> [Type a] -> IO [Type a]

simplTypes a wh ctx ts
 = mapM (simplType a wh ctx) ts


-- | Reduce the expected types in a Mode
simplMode
        :: Annot a => a -> [Where a] -> Context a
        -> Mode a -> IO (Mode a)

simplMode a wh ctx mode
 = case mode of
        Check ts
         -> do  ts'     <- simplTypes a wh ctx ts
                return  $ Check ts'

        _ -> return mode


---------------------------------------------------------------------------------------------------
-- | Flatten nested TSums in a type.
flattenType :: Type a -> Type a
flattenType tt
 = case parts tt of
        []      -> TPure
        [t]     -> t
        ts      -> TSum ts
 where
        parts (TSum ts) = concatMap parts ts
        parts TPure     = []
        parts t         = [t]


---------------------------------------------------------------------------------------------------
-- | Strip params.
stripTermParamsOfType
        :: Annot a => Context a -> Type a
        -> IO [Either [(Bind, Kind a)] [Type a]]

stripTermParamsOfType ctx tt
 = case tt of
        TAnn _ t
         -> stripTermParamsOfType ctx t

        TForall bks tBody
         -> do  rest    <- stripTermParamsOfType ctx tBody
                return  $ Left bks : rest

        TFun tsParam [tBody]
         -> do  rest    <- stripTermParamsOfType ctx tBody
                return  $ Right tsParam : rest

        TFun tsParam _tsBody
         -> do  return  $ [Right tsParam]

        _ -> return []

