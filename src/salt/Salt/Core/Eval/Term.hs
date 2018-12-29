
module Salt.Core.Eval.Term where
import Salt.Core.Eval.Error
import Salt.Core.Eval.State
import Salt.Core.Analysis.Support       ()
import Salt.Core.Transform.Snv
import Salt.Core.Transform.MapAnnot
import Control.Exception
import Control.Monad
import qualified Salt.Core.Prim.Ops     as Ops
import qualified Data.Map               as Map
import qualified Data.Set               as Set


---------------------------------------------------------------------------------------------------
-- | The usual shape of evaluation functions.
type Eval a x y
        = Annot a => State a -> a -> Env a -> x -> IO y


---------------------------------------------------------------------------------------------------
-- | Evaluate a term in the given environment.
--
--   This is a definitional interpreter that evaluates types as well as terms
--   in the input expression. We re-evaluate top-level CAFs. We explicitly check
--   for ill-formed terms, such as when we don't have the same number of field
--   names as field terms in a record construction. We also directly evaluate
--   expressions that have bumped variables, which requires deep traversal to
--   apply `Ups` variable lifting maps when the expressions are carried under
--   binders. A fast, production interpreter would be written differently.
--
evalTerm :: Eval a (Term a) [Value a]

-- (ev-ann) -----------------------------------------------
evalTerm s _a env (MAnn a' m)
 = evalTerm s a' env m

-- (ev-mmm) -----------------------------------------------
evalTerm s a env (MTerms ms)
 = evalTerms s a env ms


-- (ev-the) -----------------------------------------------
evalTerm s a env (MThe _ m)
 = evalTerm s a env m


-- (ev-val) -----------------------------------------------
evalTerm _s _ _  (MVal v)
 = return [v]


-- (ev-var) -----------------------------------------------
evalTerm s a env (MVar u)
 = resolveTermBound (stateModule s) env u
 >>= \case
        -- Value is bound in the local environment.
        Just (TermLocal v) -> return [v]

        -- Term is bound at top level.
        --   We allow terms to be bound at top level which are not already
        --   in normal form, so we need to evaluate them here.
        Just (TermDecl m)  -> evalTerm s a envEmpty m

        Nothing -> throw $ ErrorVarUnbound a u env


-- (ev-abt) -----------------------------------------------
evalTerm _s _a env (MAbt bks mBody)
 =      return [VClosure (Closure env (MPTypes bks) mBody)]


-- (ev-abm) -----------------------------------------------
evalTerm _s _a env (MAbm bts mBody)
 =      return [VClosure (Closure env (MPTerms bts) mBody)]


-- (ev-aps-prim) ------------------------------------------
evalTerm s a env (MAps (MPrm nPrim) mgssArg)
 = case Map.lookup nPrim Ops.primOps of
        Just (Ops.PP _name _type step _docs)
         -> do  nssArg   <- mapM (evalTermArgs s a env) mgssArg
                let vsResult = step nssArg
                return vsResult

        Just (Ops.PO _name _type _effs exec _docs)
         -> do  nssArg   <- mapM (evalTermArgs s a env) mgssArg
                vsResult <- exec nssArg
                return vsResult

        Nothing -> throw $ ErrorPrimUnknown a nPrim


-- (ev-aps) -----------------------------------------------
evalTerm s a env (MApp mFun mgs)
 = do   vsCloTerm <- evalTerm s a env mFun
        case vsCloTerm of
         [VClosure (Closure env' mps@(MPTerms bts) mBody)]
          -> case mgs of
                MGTerm m
                 -> do  let bs  = map fst bts
                        vsArg   <- evalTerm s a env m
                        when (not $ length vsArg == length bs)
                         $ throw $ ErrorWrongTermArity a (length bs) vsArg

                        -- TODO: add tests to ensure evaluation with bumped vars
                        -- works out. The env needs to be extended, and substitution
                        -- build from the env the right way around.
                        let env'' = envExtendValues (zip bs vsArg) env'
                        evalTerm s a env'' mBody

                MGTerms ms
                 -> do  let bs  = map fst bts
                        vsArg   <- mapM (evalTerm1 s a env) ms
                        when (not $ length vsArg == length bs)
                         $ throw $ ErrorWrongTermArity a (length bs) vsArg

                        let env'' = envExtendValues (zip bs vsArg) env'
                        evalTerm s a env'' mBody

                _ -> throw $ ErrorAppTermWrongArgs a mps mgs

         [VClosure (Closure env' mps@(MPTypes bks) mBody)]
          -> case mgs of
                MGTypes ts
                 -> do  let bs  = map fst bks
                        when (not $ length ts == length bs)
                         $ throw $ ErrorWrongTypeArity a (length bs) ts

                        let env'' = envExtendTypes (zip bs ts) env'
                        evalTerm s a env'' mBody

                _ -> throw $ ErrorAppTermWrongArgs a mps mgs

         _  -> throw $ ErrorAppTermBadClosure a vsCloTerm


-- (ev-let) -----------------------------------------------
evalTerm s a env (MLet bts mBind mBody)
 = do   vsBind <- evalTerm s a env mBind
        let nWanted = length bts
        let nHave   = length vsBind
        if  nWanted == nHave
         then do
                let bs = map fst bts
                let env' =  envExtendValues (zip bs vsBind) env
                vsResult <- evalTerm s a env' mBody
                return vsResult
         else throw $ ErrorWrongTermArity a nWanted vsBind


-- (ev-ifs) -----------------------------------------------
evalTerm s a env mm@(MKey MKIf [MGTerms msCond, MGTerms msThen, MGTerm mElse])
 = loop msCond msThen
 where
        -- Try all the conditions from top to bottom.
        loop (mCond : msCond') (mThen : msThen')
         = do   vCond   <- evalTerm1 s a env mCond
                case vCond of
                 VBool True   -> evalTerm s a env mThen
                 VBool False  -> loop msCond' msThen'
                 _            -> throw $ ErrorIfsScrutNotBool a vCond

        -- No condition evaluated to true, so run the else branch.
        loop [] []
         = do   evalTerm s a env mElse

        -- We have a different number of condition and branch terms.
        loop _ _
         = throw $ ErrorInvalidConstruct a mm


-- (ev-rec) -----------------------------------------------
evalTerm s a env (MRecord nsField msArg)
 | length nsField == length msArg
 = do   vssArg <- mapM (evalTerm s a env) msArg
        return  [VRecord $ zip nsField vssArg]


-- (ev-prj) -----------------------------------------------
evalTerm s a env (MProject nField mRecord)
 = do   vRec  <- evalTerm1 s a env mRecord
        case vRec of
         VRecord nvs
          -> case lookup nField nvs of
                Nothing -> throw $ ErrorProjectMissingField a vRec nField
                Just vs -> return vs
         _ -> throw $ ErrorProjectNotRecord a vRec nField

-- TODO: add ev-vnt, and tests for variant and case.

-- (ev-cse) -----------------------------------------------
evalTerm s a env mm@(MVarCase mScrut msAlt0)
 = do   vScrut  <- evalTerm1 s a env mScrut

        let (nScrut, vsData)
             = case vScrut of
                VVariant l _ vs -> (l, vs)
                _ -> throw $ ErrorCaseScrutNotVariant a vScrut

        let go (MVarAlt nAlt btsPat mBody : msAlt)
                |  nAlt == nScrut = (btsPat, mBody)
                |  otherwise      = go msAlt
            go [] = throw $ ErrorCaseNoMatch a vScrut
            go _  = throw $ ErrorInvalidConstruct a mm

        let (btsPat, mBody) = go msAlt0

        when (not $ length btsPat == length vsData)
         $ throw $ ErrorWrongTermArity a (length btsPat) vsData

        let bs   = map fst btsPat
        let env' = envExtendValues (zip bs vsData) env

        evalTerm s a env' mBody


-- (ev-box) -----------------------------------------------
-- TODO: add tests for ev-box.
evalTerm _s _a env (MBox mBody)
 =      return  [VClosure (Closure env (MPTerms []) mBody)]


-- (ev-run) -----------------------------------------------
-- TODO: add tests for ev-run.
evalTerm s a env (MRun mSusp)
 = do   vSusp <- evalTerm1 s a env mSusp
        case vSusp of
         VClosure (Closure env' (MPTerms []) mBody)
           -> evalTerm s a env' mBody
         _ -> throw $ ErrorRunNotSuspension a vSusp


-- (ev-lst) -----------------------------------------------
evalTerm s a env (MList t ms)
 = do   let t'  = snvApplyType upsEmpty (snvOfEnvTypes env) t
        vs      <- evalTerms s a env ms
        return [VList t' vs]


-- (ev-set) -----------------------------------------------
evalTerm s a env (MSet t ms)
 = do   let t'  = snvApplyType upsEmpty (snvOfEnvTypes env) t
        vs      <- evalTerms s a env ms
        let vs' = Set.fromList $ map stripAnnot vs
        return [VSet t' vs']


-- (ev-map) -----------------------------------------------
evalTerm s a env mm@(MMap tk tv msk msv)
 = do   let snv = snvOfEnvTypes env
        let tk' = snvApplyType upsEmpty snv tk
        let tv' = snvApplyType upsEmpty snv tv
        vsElem  <- evalPairs msk msv
        return [ VMap tk' tv' $ Map.fromList vsElem ]

 where
        -- Evaluate terms for pairs of keys and values in lockstep so that any
        -- effects are caused side by side instead of all the keys and then
        -- all the values.
        evalPairs (mk : msk') (mv : msv')
         = do   vk <- evalTerm1 s a env mk
                vv <- evalTerm1 s a env mv
                ps <- evalPairs msk' msv'
                return ((stripAnnot vk, vv) : ps)

        evalPairs [] [] = return []

        -- We were expecting the same number of keys as values.
        evalPairs _ _   = throw $ ErrorInvalidConstruct a mm


-----------------------------------------------------------
-- No match.
evalTerm _s a _ mm
 =      throw $ ErrorInvalidConstruct a mm


---------------------------------------------------------------------------------------------------
-- | Like `evalTerm`, but expect a single result value.
evalTerm1 :: Eval a (Term a) (Value a)
evalTerm1 s a env m
 = do   vs      <- evalTerm s a env m
        case vs of
         [v]    -> return v
         _      -> throw $ ErrorWrongTermArity a 1 vs


-- | Evaluate a list of terms, producing a single value for each.
evalTerms :: Eval a [Term a] [Value a]
evalTerms s a env ms
 = mapM (evalTerm1 s a env) ms


---------------------------------------------------------------------------------------------------
evalTermArgs :: Eval a (TermArgs a) (TermNormals a)
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

