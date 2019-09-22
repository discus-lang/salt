
module Salt.Core.Eval.Term where
import Salt.Core.Eval.Type
import Salt.Core.Eval.Error
import Salt.Core.Eval.Base
import Salt.Core.Analysis.Support       ()
import Salt.Core.Transform.StripAnnot
import qualified Salt.Core.Prim.Ops     as Ops

import Control.Exception
import Control.Monad
import qualified Data.Map               as Map
import qualified Data.Set               as Set


------------------------------------------------------------------------------------------- Term --
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
evalTerm :: forall a. EvalTerm a (Term a) [Value a]

-- (evm-ann) -----------------------------------------------
evalTerm s _a env (MAnn a' m)
 = do   evalTerm s a' env m


-- (evm-val) -----------------------------------------------
evalTerm _s _ _  (MVal v)
 = return [v]


-- (evm-var) -----------------------------------------------
evalTerm s a env (MVar u)
 = resolveTermBound (stateModule s) env u
 >>= \case
        -- Value is bound in the local environment.
        Just (TermDefLocal v)
         -> case v of
                -- Cell locations are automatically dereferenced,
                -- which is a difference from the usual ML-style Ref types.
                VLoc _ iCell
                 -> do  mv <- readCell s iCell
                        case mv of
                         Nothing  -> throw $ ErrorTermCellBroken a iCell env
                         Just v'  -> return [v']

                _ -> return [v]

        -- Term is bound at top level.
        --   We allow terms to be bound at top level which are not already
        --   in normal form, so we need to evaluate them here.
        Just (TermDefDecl m)  -> evalTerm s a menvEmpty m

        -- Can't find the binding site for this bound variable.
        _ -> throw $ ErrorTermVarUnbound a u env


-- (evm-abs) -----------------------------------------------
-- The environment scopes over the parameter kinds, so we don't need to evaluate them.
evalTerm _s _a env (MAbs mps mBody)
 = case takeTermParams mps of
        Left bks  -> return [VClosure (TermClosure env (MPTypes bks) mBody)]
        Right bts -> return [VClosure (TermClosure env (MPTerms bts) mBody)]


-- (evm-mmm) -----------------------------------------------
evalTerm s a env (MTerms ms)
 = evalTerms s a env ms


-- (evm-the) -----------------------------------------------
evalTerm s a env (MThe _ m)
 = evalTerm s a env m


-- (evm-aps-prim) ------------------------------------------
evalTerm s a env (MAps mFun mgssArg)
 | Just "bundle'new" <- takeMPrm mFun
 = do   [NVs [VSet _ vsType, VSet _ vsTerm]]
         <- mapM (evalTermArgs s a env) mgssArg

        let Just nsType = sequence $ map takeVSymbol $ Set.toList vsType
        let Just nsTerm = sequence $ map takeVSymbol $ Set.toList vsTerm
        evalBundleNew s a env (nsType, nsTerm)

 | Just nPrim <- takeMPrm mFun
 = do case Map.lookup nPrim Ops.primOps of
        Just (Ops.PP _name _type step _docs)
         -> do  nssArg <- mapM (evalTermArgs s a env) mgssArg
                return $ step nssArg

        Just (Ops.PO _name _type exec _docs)
         -> do  nssArg <- mapM (evalTermArgs s a env) mgssArg
                exec nssArg

        Nothing -> throw $ ErrorPrimUnknown a nPrim


-- (evm-aps) -----------------------------------------------
evalTerm s a env (MAps mFun mgssArg)
 = do   vsClo <- evalTerm s a env mFun
        nsArg <- mapM (evalTermArgs s a env) mgssArg

        case vsClo of
         [vClo] -> evalTermApp s a vClo nsArg
         _      -> throw $ ErrorAppTermBadClosure a vsClo


-- (evm-let) -----------------------------------------------
evalTerm s a env (MLet mps mBind mBody)
 | Just bts <- takeMPTerms mps
 = do   vsBind <- evalTerm s a env mBind
        let nWanted = length bts
        let nHave   = length vsBind
        if  nWanted == nHave
         then do
                let bs = map fst bts
                let env' =  menvExtendValues (zip bs vsBind) env
                vsResult <- evalTerm s a env' mBody
                return vsResult
         else throw $ ErrorWrongTermArity a nWanted vsBind


-- (evm-rec) -----------------------------------------------
evalTerm s a env mRec@(MRec bms mBody)
 = do
        -- Helper to build an abstraction that wraps the body
        -- with any remaining parameters.
        let wrap []   m = m
            wrap mpss m = foldr MAbs m mpss

        -- Make a closure for each of the bindings.
        -- Later, when we look the closure up from the environment we'll re-add
        -- the values in the recursive group back to its own environment.
        let makeClosure (MBind _ [] _ _)
             = throw $ ErrorInvalidTerm a mRec

            makeClosure (MBind b (mps : mpss) _tResult mBody')
             = case takeTermParams mps of
                Left  bks -> (b, TermClosure env (MPTypes bks) $ wrap mpss mBody')
                Right bts -> (b, TermClosure env (MPTerms bts) $ wrap mpss mBody')

        let env' = menvExtendValuesRec (map makeClosure bms) env
        evalTerm s a env' mBody


-- (evm-ifs) -----------------------------------------------
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
         = throw $ ErrorInvalidTerm a mm


-- (evm-rcd) -----------------------------------------------
evalTerm s a env (MRecord nsField msArg)
 | length nsField == length msArg
 = do   vssArg <- mapM (evalTerm s a env) msArg
        return  [VRecord $ zip nsField vssArg]


-- (evm-prj) -----------------------------------------------
evalTerm s a env (MProject nField mRecord)
 = do   vRec  <- evalTerm1 s a env mRecord
        case vRec of
         VRecord nvs
          -> case lookup nField nvs of
                Nothing -> throw $ ErrorProjectMissingField a vRec nField
                Just vs -> return vs
         _ -> throw $ ErrorProjectNotRecord a vRec nField


-- (evm-vnt) -----------------------------------------------
evalTerm s a env (MVariant l m t)
 = do   let tenv = menvSliceTypeEnv env
        t'      <- evalType s a tenv t
        vsField <- evalTerm s a env  m
        return [VVariant l t' vsField]


-- (evm-cse) -----------------------------------------------
evalTerm s a env mm@(MVarCase mScrut msAlt0 msElse)
 | length msElse <= 1
 = do   vScrut  <- evalTerm1 s a env mScrut

        let (nScrut, vsData)
             = case vScrut of
                VVariant l _ vs -> (l, vs)
                _ -> throw $ ErrorCaseScrutNotVariant a vScrut

        let go (MVarAlt nAlt mpsPat mBody : msAlt)
                | Just btsPat   <- takeMPTerms mpsPat
                , nAlt == nScrut = Just (btsPat, mBody)
                |  otherwise     = go msAlt

            go [] = Nothing
            go _  = throw $ ErrorInvalidTerm a mm

        case go msAlt0 of
         -- We have a matching alternative.
         Just (btsPat, mBody)
          -> do when (not $ length btsPat == length vsData)
                 $ throw $ ErrorWrongTermArity a (length btsPat) vsData
                let bs   = map fst btsPat
                let env' = menvExtendValues (zip bs vsData) env
                evalTerm s a env' mBody

         Nothing
          -> case listToMaybe msElse of
                -- We don't have an alternative that matches the value,
                -- but we do have a default 'else' alternative.
                Just mElse  -> evalTerm s a env mElse

                -- There isn't a default alternative, so we're done here.
                Nothing     -> throw $ ErrorCaseNoMatch a vScrut


-- (evm-box) -----------------------------------------------
evalTerm _s _a env (MBox mBody)
 =      return  [VClosure (TermClosure env (MPTerms []) mBody)]


-- (evm-run) -----------------------------------------------
evalTerm s a env (MRun mSusp)
 = do   vSusp <- evalTerm1 s a env mSusp
        case vSusp of
         VClosure (TermClosure env' (MPTerms []) mBody)
           -> evalTerm s a env' mBody
         _ -> throw $ ErrorRunNotSuspension a vSusp


-- (evm-lst) -----------------------------------------------
evalTerm s a env (MList t ms)
 = do   let tenv = menvSliceTypeEnv env
        t'      <- evalType  s a tenv t
        vs      <- evalTerms s a env  ms
        return [VList t' vs]


-- (evm-set) -----------------------------------------------
evalTerm s a env (MSet t ms)
 = do   let tenv = menvSliceTypeEnv env
        t'      <- evalType  s a tenv t
        vs      <- evalTerms s a env  ms
        let vs' = Set.fromList $ map stripAnnot vs
        return [VSet t' vs']


-- (evm-map) -----------------------------------------------
evalTerm s a env mm@(MMap tk tv msk msv)
 = do   let tenv = menvSliceTypeEnv env
        tk'     <- evalType s a tenv tk
        tv'     <- evalType s a tenv tv
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
        evalPairs _ _   = throw $ ErrorInvalidTerm a mm


-- (evm-seq) ----------------------------------------------
evalTerm s a env (MSeq mps mBind mRest)
 = evalTerm s a env (MLet mps mBind mRest)


-- (evm-launch) -------------------------------------------
evalTerm s a env (MLaunch _tsRet mBody)
 = catch eval handle'
 where
        eval
         = do   evalTerm s a env mBody

        handle' (e :: EvalControl a)
         = case e of
                EvalControlReturn vs -> return vs
                EvalControlBreak     -> throw $ ErrorLaunchBreak a
                EvalControlContinue  -> throw $ ErrorLaunchContinue a
                EvalControlLeave     -> throw $ ErrorLaunchLeave a


-- (evm-return) -------------------------------------------
evalTerm s a env (MReturn mBody)
 = do   vs <- evalTerm s a env mBody
        throw (EvalControlReturn vs)


-- (evm-cell) ---------------------------------------------
evalTerm s a env (MCell nCell tCell mInit mRest)
 = do
        -- Evaluate the term to produce the initial value of the cell.
        vInit   <- evalTerm1 s a env mInit

        -- Allocate a new cell identifier and write the value.
        iCell   <- newCell s
        writeCell s iCell vInit

        -- The environment maps the name of the cell to its identifier,
        -- which is automatically dereferenced if we use the cell name
        -- as a bound occurrence of a variable.
        let env' = menvExtendValue (BindName nCell) (VLoc tCell iCell) env

        -- Evaluate the body of the procedure.
        vs      <- evalTerm s a env' mRest

        -- The cell is automatically deleted when it goes out of scope.
        -- There is no chance of the cell escaping this scope as cell
        -- locations are not first class values -- we automatically
        -- dereference them as mentioend above.
        delCell s iCell

        return vs


-- (evm-update) -------------------------------------------
evalTerm s a env (MUpdate nCell mNew mRest)
 = do
        -- Lookup the cell identifier.
        iCell   <- resolveTermBound (stateModule s) env (Bound nCell)
                >>= \case
                        Just (TermDefLocal (VLoc _ i)) -> return i
                        _ -> throw $ ErrorTermCellUnbound a nCell env

        -- Evaluate the new cell contents.
        vNew    <- evalTerm1 s a env mNew

        -- Update the cell.
        writeCell s iCell vNew

        -- Evaluate the rest of the procedure.
        evalTerm s a env mRest


-- (evm-whens) --------------------------------------------
evalTerm s a env mm@(MWhens msCond msThen mRest)
 = go msCond msThen
 where
        go [] []
         = evalTerm s a env mRest

        go (mCond : msCond') (mThen : msThen')
         = do   vs <- evalTerm s a env mCond
                case vs of
                 [VBool b]
                  | b           -> evalTerm s a env mThen
                  | otherwise   -> go msCond' msThen'
                 _              -> throw $ ErrorInvalidTerm a mm

        go _ _ = throw $ ErrorInvalidTerm a mm


-- (evm-loop) ---------------------------------------------
evalTerm s a env mm@(MLoop mBody mRest)
 = catch evalLoop handleLoop
 where
        evalLoop
         = do   vs <- evalTerm s a env mBody
                case vs of
                 []     -> evalLoop
                 _      -> throw $ ErrorInvalidTerm a mm

        handleLoop (e :: EvalControl a)
         = case e of
                EvalControlReturn{}  -> throw e
                EvalControlBreak     -> evalTerm s a env mRest
                EvalControlContinue  -> evalLoop
                EvalControlLeave     -> throw e


-- (evm-break) --------------------------------------------
evalTerm _s _a _env MBreak
 = throw $ EvalControlBreak @a


-- (evm-continue) -----------------------------------------
evalTerm _s _a _env MContinue
 = throw $ EvalControlContinue @a


-- (evm-while) --------------------------------------------
evalTerm s a env mm@(MWhile mPred mBody mRest)
 = catch evalLoop handleLoop
 where
        evalLoop
         = do   vs <- evalTerm s a env mPred
                case vs of
                 [VBool False]
                  -> evalTerm s a env mRest

                 [VBool True]
                  -> do vs' <- evalTerm s a env mBody
                        case vs' of
                         []     -> evalLoop
                         _      -> throw $ ErrorInvalidTerm a mm

                 _ -> throw $ ErrorInvalidTerm a mm

        handleLoop (e :: EvalControl a)
         = case e of
                EvalControlReturn{} -> throw e
                EvalControlBreak    -> evalTerm s a env mBody
                EvalControlContinue -> evalLoop
                EvalControlLeave    -> throw e


-- (evm-enter) --------------------------------------------
evalTerm s a env mEnter@(MEnter mFirst bms mRest)
 = do
     -- Helper to build an abstraction that wraps the body
        -- with any remaining parameters.
        let wrap []   m = m
            wrap mpss m = foldr MAbs m mpss

        -- Make a closure for each of the bindings.
        -- Later, when we look the closure up from the environment we'll re-add
        -- the values in the recursive group back to its own environment.
        let makeClosure (MBind _ [] _ _)
             = throw $ ErrorInvalidTerm a mEnter

            makeClosure (MBind b (mps : mpss) _tResult mBody')
             = case takeTermParams mps of
                Left  bks -> (b, TermClosure env (MPTypes bks) $ wrap mpss mBody')
                Right bts -> (b, TermClosure env (MPTerms bts) $ wrap mpss mBody')

        -- Enter the first binding, with all the others in scope.
        let env'      = menvExtendValuesRec (map makeClosure bms) env
        let evalFirst = evalTerm s a env' mFirst
        let handleLeave (e :: EvalControl a)
             = case e of
                EvalControlReturn{}     -> throw e
                EvalControlBreak{}      -> throw e
                EvalControlContinue{}   -> throw e
                EvalControlLeave        -> return []

        [] <- catch evalFirst handleLeave

        -- Once we've left the loop then continue with the original
        -- environment, so the recursive bindings are no longer in scope.
        evalTerm s a env mRest


-- (evm-leave) --------------------------------------------
evalTerm _s _a _env MLeave
 = throw $ EvalControlLeave @a


-- (evm-private) -----------------------------------------
evalTerm s a env (MPrivate bksR _ mBody)
 = do let bsR = map fst bksR
      let tsR = map snd bksR
      let tenv = menvSliceTypeEnv env
      tsR' <- mapM (evalType s a tenv) tsR
      let env' = menvExtendTypes (zip bsR tsR') env

      -- FIXME TODO need to add witnesses to env

      evalTerm s a env' mBody


-- (evm-extend) ------------------------------------------
evalTerm s a env (MExtend _ bksR _ mBody)
 = do let bsR = map fst bksR
      let tsR = map snd bksR
      let tenv = menvSliceTypeEnv env
      tsR' <- mapM (evalType s a tenv) tsR
      let env' = menvExtendTypes (zip bsR tsR') env

      -- FIXME TODO need to add witnesses to env

      evalTerm s a env' mBody

-- (evm-pack) --------------------------------------------
evalTerm s a env (MPack term abstractedTypes ascription)
 = do let tenv = menvSliceTypeEnv env
      termVal   <- evalTerm1 s a env term
      termTypes <- mapM (evalType s a tenv) abstractedTypes
      ascType   <- evalType s a tenv ascription
      let val = VExtPair termVal termTypes ascType
      return [val]

-- (evm-unpack) ------------------------------------------
evalTerm s a env (MUnpack mPacked rTermBinding rTypeBindings mBody)
 = do
      -- evaluate mPacked which should resolve to a value of VExtPair
      mPacked' <- evalTerm1 s a env mPacked
      (mPackedVal, mPackedTypes) <- case mPacked' of
        (VExtPair val ts _)  -> return (val, ts)
        _                    -> throw $ ErrorUnpackAppliedToNotPack a mPacked
      -- unpack rTypeBindings
      let rTypeBindings' = [ n | (n, _) <- rTypeBindings]
      -- repack type bindings with actual packed types
      let typeBindings = zip rTypeBindings' mPackedTypes

      -- bind rTypeBindings to actual type
      let env'  = menvExtendTypes typeBindings env

      -- unpack term binding
      let (termBinding, _) = rTermBinding

      -- bind rTermBinding to packed value
      let env'' = menvExtendValue termBinding mPackedVal env'

      -- evaluate body in new env
      evalTerm s a env'' mBody

-----------------------------------------------------------
-- No match.
evalTerm _s a _env mm
 =      throw $ ErrorInvalidTerm a mm


-- | Evaluate Like `evalTerm`, but expect a single result value.
evalTerm1 :: EvalTerm a (Term a) (Value a)
evalTerm1 s a env m
 = do   vs      <- evalTerm s a env m
        case vs of
         [v]    -> return v
         _      -> throw $ ErrorWrongTermArity a 1 vs


-- | Evaluate a list of terms, producing a single value for each.
evalTerms :: EvalTerm a [Term a] [Value a]
evalTerms s a env ms
 = mapM (evalTerm1 s a env) ms


--------------------------------------------------------------------------------------- TermArgs --
-- | Evaluate term arguments to normal form.
evalTermArgs :: EvalTerm a (TermArgs a) (TermNormals a)
evalTermArgs s a env mgs
 = case mgs of
        MGAnn _ mgs'
         -> evalTermArgs s a env mgs'

        MGTerm  m
         -> do  vs  <- evalTerm s a env m
                return $ NVs vs

        MGTerms ms
         -> do  vs  <- mapM (evalTerm1 s a env) ms
                return $ NVs vs

        MGTypes ts
         -> do  let tenv = menvSliceTypeEnv env
                ts' <- mapM (evalType s a tenv) ts
                return $ NTs ts'


-------------------------------------------------------------------------------------------- App --
-- | Apply a closure to its normal form arguments.
--
--   This handles application in 'spine form', where we have a list of
--   arguments instead of many individual application nodes.
--
evalTermApp
        :: Annot a
        => State a -> a
        -> Value a -> [TermNormals a] -> IO [Value a]

evalTermApp s a
        (VClosure (TermClosure env' (MPTerms bts) mBody))
        (NVs vsArg : nssRest)
 = do
        let bs  = map fst bts
        when (not $ length vsArg == length bs)
         $ throw $ ErrorWrongTermArity a (length bs) vsArg

        let env'' = menvExtendValues (zip bs vsArg) env'
        vsBody <- evalTerm s a env'' mBody
        case (vsBody, nssRest) of
         (vs,     [])   -> return vs
         ([vClo], nss') -> evalTermApp s a vClo nss'
         _              -> throw $ ErrorAppTermBadClosure a vsBody

evalTermApp s a
        (VClosure (TermClosure env' (MPTypes bks) mBody))
        (NTs tsArg : nssRest)
 = do
        let bs  = map fst bks
        when (not $ length tsArg == length bs)
         $ throw $ ErrorWrongTypeArity a (length bs) tsArg

        let env'' = menvExtendTypes (zip bs tsArg) env'
        vsBody <- evalTerm s a env'' mBody
        case (vsBody, nssRest) of
         (vs,     [])   -> return vs
         ([vClo], nss') -> evalTermApp s a vClo nss'
         _              -> throw $ ErrorAppTermBadClosure a vsBody

evalTermApp _s a (VClosure (TermClosure _ mps _mBody)) (ns : _)
 = throw $ ErrorAppTermMismatch a mps ns

evalTermApp _s a vClo _
 = throw $ ErrorAppTermBadClosure a [vClo]


----------------------------------------------------------------------------------------- Bundle --
-- | Evaluate an application of #bundle'new which reifies
--   declarations from the context into a value.
--   TODO: this is incomplete.
evalBundleNew :: EvalTerm a ([Name], [Name]) [Value a]
evalBundleNew s _a _env (_nsType, nsTerm)
 = do
--        let ndsType
--                = Map.fromList [ (declName n, d) | d@DeclType{} <- stateModule s ]

        let ndsTerm
                = Map.fromList
                $ [ (n, d) |  DTerm d@(DeclTerm _ _ n _ _ _)
                           <- moduleDecls $ stateModule s ]

        -- Cut the environment back to just contain the top level decls.
        let getTermBind n
             = case Map.lookup n ndsTerm of
                Just (DeclTerm a DeclTermModePlain n' tgs tResult mBody)
                 |  n == n'
                 -> (n, BundleTerm a n tgs tResult mBody)

                Just (DeclTerm a DeclTermModeProc  n' tgs tsResult mBody)
                 |  n == n'
                 -> (n, BundleTerm a n tgs tsResult (MLaunch tsResult mBody))

                _ -> error "evalBundleNew: cannot find name"

        let nmsTerm = map getTermBind nsTerm

        return [VBundle (Bundle Map.empty (Map.fromList nmsTerm))]

