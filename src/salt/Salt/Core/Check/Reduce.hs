
module Salt.Core.Check.Reduce where
import Salt.Core.Check.Context
import Salt.Core.Transform.Snv
import Salt.Core.Exp
import qualified Data.Set       as Set


---------------------------------------------------------------------------------------------------
-- | Reduce a type to weak head normal form.
--   If the head is also an effect type then normalize it.
reduceType
        :: Annot a => a -> Context a
        -> Type a -> IO (Maybe (Type a))

reduceType a ctx (TAnn _a t)
 = reduceType a ctx t
 >>= \case
        Nothing -> return $ Just t
        Just t' -> return $ Just t'

reduceType a ctx tt@(TVar u)
 = contextResolveTypeBound ctx [] u
 >>= \case
       Just (TypeDecl _k tSyn)
         ->  reduceType a ctx tSyn
         >>= \case
               Just tRed  -> return $ Just tRed
               Nothing    -> return $ Just tSyn

       _ -> return $ Just tt

reduceType _a _ctx tt@(TSum{})
 = return $ flattenType tt

reduceType a ctx (TApp tFun tgsArgs)
 = simplType a ctx tFun
 >>= \case
        TAbs tpsParam tBody
         | bksParam <- takeTPTypes tpsParam
         , tsArgs   <- takeTGTypes tgsArgs
         , length bksParam == length tsArgs
         -> do  let ns     = [n | BindName n <- map fst bksParam]
                let snv    = snvOfBinds $ zip ns tsArgs
                let tBody' = snvApplyType upsEmpty snv tBody
                reduceType a ctx tBody'
                 >>= \case
                        Just tRed -> return $ Just tRed
                        Nothing   -> return $ Just tBody'

        _ -> return Nothing

reduceType _ _ _
 =      return Nothing


---------------------------------------------------------------------------------------------------
-- | Like `reduceType` but just return the original type if it cannot be
--   reduced further.
simplType
        :: Annot a => a -> Context a
        -> Type a -> IO (Type a)

simplType a ctx tt
 = reduceType a ctx tt
 >>= \case
        Nothing  -> return tt
        Just tt' -> return tt'


-- | Simplify all the given types to head normal form.
--   We look through annotations, unfold synonyms and reduce type applications
--   to reveal the head constructor.
simplTypes
        :: Annot a => a -> Context a
        -> [Type a] -> IO [Type a]

simplTypes a ctx ts
 = mapM (simplType a ctx) ts


---------------------------------------------------------------------------------------------------
-- | Flatten TSums at the head of a type.
--
--   * We flatten nested TSums
--   * Drop TPure inside TSum
--   * Drop repeated TPrms and TCons in TSums
--   * Return `Nothing` if we didn't need to do anything.
--
flattenType :: Type a -> Maybe (Type a)
flattenType tt
 = goStart tt
 where
        goStart (TSum ts)
         = goParts False Set.empty Set.empty [] ts

        goStart _
         = Nothing

        goParts bMoved nsPrm nsCon acc (t : tsMore)
         = case t of
            TAnn _ t'             -> goParts bMoved nsPrm nsCon acc (t' : tsMore)
            TPure                 -> goParts True   nsPrm nsCon acc tsMore
            TSum ts'              -> goParts True   nsPrm nsCon acc (ts' ++ tsMore)

            TCon n
             | Set.member n nsCon -> goParts True   nsPrm nsCon acc tsMore
             | otherwise          -> goParts bMoved nsPrm (Set.insert n nsCon) acc tsMore

            TPrm n
             | Set.member n nsPrm -> goParts True   nsPrm nsCon acc tsMore
             | otherwise          -> goParts bMoved (Set.insert n nsPrm) nsCon acc tsMore

            _                     -> goParts bMoved nsPrm nsCon (t : acc) tsMore

        goParts bMoved nsPrm nsCon acc []
         | not bMoved
         = Nothing

         | otherwise
         = goEmit (  (map TPrm $ Set.toList nsPrm)
                  ++ (map TCon $ Set.toList nsCon)
                  ++ reverse acc)

        goEmit acc
         = case acc of
                []      -> Just TPure
                [t]     -> Just t
                _       -> Just $ TSum acc


---------------------------------------------------------------------------------------------------
-- | Strip params.
stripTermParamsOfType
        :: Annot a => Context a -> Type a
        -> IO [Either [(Bind, Kind a)] [Type a]]

stripTermParamsOfType ctx tt
 = case tt of
        TAnn _ t
         -> stripTermParamsOfType ctx t

        TForall tps tBody
         -> do  rest    <- stripTermParamsOfType ctx tBody
                return  $ Left (takeTPTypes tps) : rest

        TFun tsParam [tBody]
         -> do  rest    <- stripTermParamsOfType ctx tBody
                return  $ Right tsParam : rest

        TFun tsParam _tsBody
         -> do  return  $ [Right tsParam]

        _ -> return []

