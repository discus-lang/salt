
module Salt.Core.Eval.Term where
import Salt.Core.Eval.Data
import Salt.Core.Analysis.Support
import Salt.Core.Transform.MapAnnot
import Control.Exception
import qualified Salt.Core.Prim.Ops     as Ops
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import qualified Text.Show.Pretty       as Text


---------------------------------------------------------------------------------------------------
-- | Evaluate a term in the given environment.
evalTerm :: Annot a
         => State a -> a -> Env a -> Term a -> IO [Value a]

-- Pass through annotations.
evalTerm s _a env (MAnn a' m)
 = evalTerm s a' env m


-- Multi value return.
evalTerm s a env (MKey MKTerms [MGTerms ms])
 = evalTerms s a env ms


-- Type ascription.
evalTerm s a env (MKey MKThe [MGTypes [_], MGTerm m])
 = evalTerm s a env m


-- Values.
evalTerm _s _ _  (MRef (MRVal v))
 = return [v]


-- Variables
evalTerm s a env (MVar u@(Bound n))
 -- Bound in local environment.
 | Just v       <- envLookupValue n env
 = return [v]

 -- Bound in top-level environment.
 | Just (DeclTerm _a _n mps _tr mBody)
   <- Map.lookup n (stateDeclTerms s)
 = let
        -- TODO: proper error.
        makeClosure []
         = error "no params for DeclTerm"

        makeClosure (mp : [])
         = VClosure $ Closure envEmpty mp mBody

        makeClosure (mp1 : mps')
         = VClosure $ Closure envEmpty mp1 (MVal (makeClosure mps'))

   in   return [makeClosure mps]

 | otherwise
 = throw $ ErrorVarUnbound a u env


-- Function abstraction.
--   For debugging we trim the closure environment at the point the
--   closure is produced so that the closure is easier to read in logs.
--   For a production interpreter we'd avoid the trimming.
evalTerm _s _a env (MAbm bts mBody)
 = do   let nsTermFree  = freeTermVarsOf mBody
        let Env ebs     = env

        -- TODO: also trim type binds.
        let keepEnvBind (EnvValue n _) = Set.member n nsTermFree
            keepEnvBind (EnvType  _ _) = True

        let env'        = Env (filter keepEnvBind ebs)
        return [VClosure (Closure env' (MPTerms bts) mBody)]


-- Prim-term application
evalTerm s a env (MAps (MPrm nPrim) mgssArg)
 = case Map.lookup nPrim Ops.primOps of
        Just (Ops.PP _name _type step _docs)
         -> do  nssArg   <- mapM (evalTermArgs s a env) mgssArg
                let vsResult = step nssArg
                return vsResult

        Just (Ops.PO _name _type exec _docs)
         -> do  nssArg   <- mapM (evalTermArgs s a env) mgssArg
                vsResult <- exec nssArg
                return vsResult

        Nothing -> throw $ ErrorPrimUnknown a nPrim


-- Term-term, term/type application.
evalTerm s a env (MApp mFun mgsArg)
 = do   vsCloTerm <- evalTerm s a env mFun
        case vsCloTerm of
         [VClosure (Closure env' (MPTerms bts) mBody)]
          | MGTerm m    <- mgsArg
          -> do let bs    = map fst bts
                vsArg   <- evalTerm s a env m
                let env'' = envExtendsValue (zip bs vsArg) env'
                vsRes   <- evalTerm s a env'' mBody
                return vsRes

          | MGTerms ms  <- mgsArg
          -> do let bs  = map fst bts
                vsArg   <- mapM (evalTerm1 s a env) ms
                let env'' = envExtendsValue (zip bs vsArg) env'
                vsRes   <- evalTerm s a env'' mBody
                return vsRes

          | otherwise -> error "invalid application"

         [VClosure (Closure env' (MPTypes bks) mBody)]
          | MGTypes ts   <- mgsArg
          -> do let bs  = map fst bks
                -- TODO: drop subst into tsArg'
                let tsArg' = ts
                let env'' = envExtendsType (zip bs tsArg') env'
                vsRes   <- evalTerm s a env'' mBody
                return vsRes

          | otherwise -> error "invalid application"

         [] -> throw $ ErrorAppTermNotEnough a []
         _  -> error $ Text.ppShow (a, vsCloTerm) -- throw $ ErrorAppTermTooMany   a vsCloTerm


-- Let-binding.
evalTerm s a env (MLet bts mBind mBody)
 = do   vsBind <- evalTerm s a env mBind
        let nWanted = length bts
        let nHave   = length vsBind
        if  nWanted == nHave
         then do
                let bs = map fst bts
                let env' =  envExtendsValue (zip bs vsBind) env
                vsResult <- evalTerm s a env' mBody
                return vsResult
        else if nHave < nWanted
         then throw $ ErrorAppTermNotEnough a vsBind
        else  throw $ ErrorAppTermTooMany   a vsBind


-- Data constructor application.
-- TODO: get a pattern synonym for this.
evalTerm s a env (MKey (MKCon nCon) [MGTypes ts, MGTerms msArg])
 = do   vsArg  <- evalTerms s a env msArg
        return  [VData nCon ts vsArg]


-- Record constructor application.
evalTerm s a env (MRecord nsField msArg)
 | length nsField == length msArg
 = do   vsArg <- evalTerms s a env msArg
        return [VRecord $ zip nsField vsArg]


-- Record field projection.
evalTerm s a env (MProject nField mRecord)
 = do   vRec  <- evalTerm1 s a env mRecord
        case vRec of
         VRecord nvs
          -> case lookup nField nvs of
                Nothing -> throw $ ErrorProjectMissingField a vRec nField
                Just v  -> return [v]
         _ -> throw $ ErrorProjectTypeMismatch a vRec nField


-- Case matching.
-- TODO: proper errors.
evalTerm s a env (MCase mScrut lsAlt msAlt)
 = do   vScrut  <- evalTerm1 s a env mScrut
        case vScrut of
         VVariant l vs
          -> case lookup l $ zip lsAlt msAlt of
                Nothing -> error "variant is not in alts"
                Just mAlt
                 -> do  vAlt <- evalTerm1 s a env mAlt
                        case vAlt of
                         VClosure (Closure env' (MPTerms bts) mBody)
                          -> let bs     = map fst bts
                                 env''  = envExtendsValue (zip bs vs) env'
                             in  evalTerm s a env'' mBody
                         _ -> error "alt is not a closure"
         _ -> error "scrut is not a variant"


-- If-then-else
-- TODO: proper errors.
evalTerm s a env (MKey MKIf [MGTerms msCond, MGTerms msThen, MGTerm mElse])
 = let
        loop [] []
         = do   evalTerm s a env mElse

        loop (mCond : msCond') (mThen : msThen')
         = do   vCond   <- evalTerm1 s a env mCond
                case vCond of
                 VBool True   -> evalTerm s a env mThen
                 VBool False  -> loop msCond' msThen'
                 _            -> error "if-then-else runtime type error"

        loop _ _ = error "if cond then length mismatch"

   in   loop msCond msThen


-- List construction.
evalTerm s a env (MKey MKList [MGTypes [t], MGTerms ms])
 = do   vsArg <- evalTerms s a env ms
        return [VList t vsArg]


-- Set construction.
evalTerm s a env (MKey MKSet  [MGTypes [t], MGTerms ms])
 = do   vsArg <- evalTerms s a env ms
        return [VSet t $ Set.fromList $ map stripAnnot $ vsArg]


-- No match.
evalTerm _s a _ mm
 =      throw $ ErrorInvalidConstruct a mm


---------------------------------------------------------------------------------------------------
-- | Like `evalTerm`, but expect a single result value.
evalTerm1
        :: Annot a
        => State a -> a -> Env a
        -> Term a -> IO (Value a)
evalTerm1 s a env m
 = do   vs      <- evalTerm s a env m
        case vs of
         [v]    -> return v
         []     -> throw $ ErrorAppTermNotEnough a []
         vs'    -> error $ Text.ppShow (a, vs')

--          throw $ ErrorAppTermTooMany   a vs'


-- | Evaluate a list of terms, producing a single value for each.
evalTerms
        :: Annot a
        => State a -> a -> Env a
        -> [Term a] -> IO [Value a]
evalTerms s a env ms
 = mapM (evalTerm1 s a env) ms


---------------------------------------------------------------------------------------------------
evalTermArgs
        :: Annot a
        => State a -> a -> Env a
        -> TermArgs a -> IO (TermNormals a)

evalTermArgs s a env mgs
 = case mgs of
        MGTerm  m
         -> do  vs <- evalTerm s a env m
                return $ NVs vs

        MGTerms ms
         -> do  vs <- mapM (evalTerm1 s a env) ms
                return $ NVs vs

        MGTypes ts
         -> do  -- TODO: drop env as subst. into type.
                return $ NTs ts

