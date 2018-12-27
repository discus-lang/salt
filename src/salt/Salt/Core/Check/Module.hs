
module Salt.Core.Check.Module where
import Salt.Core.Check.Error
import Salt.Core.Check.Where
import Salt.Core.Check.Type
import Salt.Core.Check.Term
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Params
import Salt.Core.Check.Type.Base
import qualified Salt.Core.Analysis.Support     as Support

import qualified Control.Exception              as Control
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


---------------------------------------------------------------------------------------------------
-- | Check a whole module.
---
--   We need to do this in stages to ensure that type synonyms are are well
--   kinded before checking terms that may mention them etc.
--
--   We want to do this so we don't accidently reduce type operator
--   applications that were actually ill-kinded.
--
checkModule
        :: Annot a
        => a -> Module a
        -> IO (Context a, Module a, [Error a])

checkModule a mm
 = goTypeSigs ctxStart (moduleDecls mm)
 where
        ctxStart
         = Context
         { contextCheckType     = checkTypeWith
         , contextCheckTerm     = checkTermWith
         , contextModuleType    = Map.empty
         , contextModuleTerm    = Map.empty
         , contextLocal         = [] }

        -- Check kind signatures on types, before adding them to the context.
        goTypeSigs  ctx decls
         = do
                -- Check kind signatures on type decls.
                (decls', errsSig)
                 <- checkDecls (checkDeclTypeSig a ctx) decls

                -- Check type decls not recursive.
                let errsRec = checkDeclTypeNonRec decls

                -- TODO: check rebound type synonyms.
                let errs    = errsSig ++ errsRec

                if not $ null errs
                 then return (ctx, mm, errs)
                 else do
                        let nktsDeclType
                                = [ ( n
                                  , ( makeDeclKindOfParamsResult pss kResult
                                    , makeTAbsOfParams pss tBody))
                                  | DType (DeclType _a n pss kResult tBody) <- decls' ]
                        let ctx' = ctx { contextModuleType = Map.fromList nktsDeclType }
                        goTypeDecls ctx' decls'

        -- Check individual type declarations.
        goTypeDecls ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclType a ctx) decls

                if null errs
                 then goTermSigs  ctx decls'
                 else return (ctx, mm, errs)

        -- Check type signatures on terms, before adding them to the context.
        goTermSigs ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclTermSig a ctx) decls

                let ntsDeclTerm
                         = [ (n, makeDeclTypeOfParamsResult pss tsResult)
                           | DTerm (DeclTerm _a n pss tsResult _mBody) <- decls' ]

                let ctx' = ctx { contextModuleTerm = Map.fromList ntsDeclTerm }
                if null errs
                 then goTermDecls ctx' decls'
                 else return (ctx, mm, errs)

        -- Check individual term declarations.
        goTermDecls ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclTerm a ctx) decls

                if null errs
                 then goTestDecls ctx decls'
                 else return (ctx, mm, errs)

        -- Check individual test declarations.
        goTestDecls ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclTest a ctx) decls

                if null errs
                 then return (ctx, mm { moduleDecls = decls' }, errs)
                 else return (ctx, mm, errs)


makeDeclKindOfParamsResult :: [TypeParams a] -> Kind a -> Kind a
makeDeclKindOfParamsResult pss0 kResult
 = loop pss0
 where
        loop [] = kResult
        loop (TPTypes bks : pss')
         = TArr (map snd bks) $ loop pss'


-- TODO: throw proper arity errors.
makeDeclTypeOfParamsResult :: [TermParams a] -> [Type a] -> Type a
makeDeclTypeOfParamsResult pss0 tsResult
 = case loop pss0 of
        [t]     -> t
        _       -> error "arity error when making decl type"
 where
        loop []                   = tsResult
        loop (MPTerms bts : pss') = [TFun (map snd bts) (loop pss')]

        loop (MPTypes bts : pss')
         = case loop pss' of
                [t] -> [TForall bts t]
                _   -> error "arity error when making decl type"


---------------------------------------------------------------------------------------------------
type CheckDecl a
        = Annot a => a -> Context a -> Decl a -> IO (Decl a)

checkDecls
        :: forall a. Annot a
        => (Decl a  -> IO (Decl a))
        -> [Decl a] -> IO ([Decl a], [Error a])

checkDecls _check []
 = return ([], [])

checkDecls check (d1 : ds2)
 = do   (d1', errs1)
         <- Control.try (check d1)
         >>= \case
                Right d1'             -> return (d1', [])
                Left (err :: Error a) -> return (d1, [err])

        (ds2', errs2) <- checkDecls check ds2
        return (d1' : ds2', errs1 ++ errs2)


---------------------------------------------------------------------------------------------------
-- | Check kind signatures of type declarations.
checkDeclTypeSig :: CheckDecl a
checkDeclTypeSig _a ctx (DType (DeclType a n tpss kResult tBody))
 = do   let wh  = [WhereTypeDecl a n]
        tpss'    <- checkTypeParamss a wh ctx tpss
        kResult' <- checkKind a wh ctx kResult

        return  $ DType $ DeclType a n tpss' kResult' tBody

checkDeclTypeSig _ _ decl
 = return decl


-- | Check bodies of type declarations.
checkDeclType :: CheckDecl a
checkDeclType _a ctx (DType (DeclType a n tpss kResult tBody))
 = do   let wh   = [WhereTypeDecl a n]
        tpss'    <- checkTypeParamss a wh ctx tpss
        kResult' <- checkKind a wh ctx kResult

        let ctx' =  foldl (flip contextBindTypeParams) ctx tpss'
        tBody'   <- checkTypeIs a wh ctx' kResult' tBody
        return  $ DType $ DeclType a n tpss' kResult' tBody'

checkDeclType _ _ decl
 = return decl


-- | Check for recursive type declarations.
checkDeclTypeNonRec :: Annot a => [Decl a] -> [Error a]
checkDeclTypeNonRec decls
 = concatMap checkDecl decls
 where
        -- Check a single declaration.
        checkDecl decl
         = case decl of
                DType (DeclType aDecl nDecl _ _ _)
                  -> checkDeps aDecl nDecl Set.empty (Set.singleton nDecl)
                _ -> []

        -- Map of names of type decls to others they depend on.
        deps    = Map.fromList
                $ [ (n, freeTypeNamesOf tBody)
                  | DType (DeclType _a n _ _ tBody) <- decls ]

        freeTypeNamesOf t
                = Set.fromList
                $ [ n | BoundWith n 0 <- Set.toList $ Support.freeTypeBoundsOf t ]

        -- Worklist algorithm to find recursive dependencies.
        --   We track the synonym bindings we have entered into,
        --   as well as the ones we still need to check.
        checkDeps aDecl nDecl nsEntered nsCheck
         = let (nsCheck1, nsRest) = Set.splitAt 1 nsCheck
           in  case Set.toList nsCheck1 of
                []      -> []
                nCheck : _
                 |  Just nsFree <- Map.lookup nCheck deps
                 ,  nsRec <- Set.intersection nsFree nsEntered
                 -> if not $ Set.null nsRec
                        then [ErrorTypeDeclsRecursive aDecl
                                [WhereTypeDecl aDecl nDecl]
                                nDecl (concatMap nameAnnotsOfTypeDecl $ Set.toList nsRec)]
                        else checkDeps aDecl nDecl
                                (Set.union nsFree nsEntered)
                                (Set.union nsRest nsFree)

                 | otherwise    -> checkDeps aDecl nDecl nsEntered nsRest

        -- Get annotations for type declarations with the given name.
        -- This is used when reporting errors due to recursive declarations.
        nameAnnotsOfTypeDecl n
         = mapMaybe (nameAnnotOfTypeDecl n) decls

        nameAnnotOfTypeDecl n decl
         = case decl of
                DType (DeclType a n' _ _ _)
                 | n == n'      -> Just (n, a)
                _               -> Nothing


---------------------------------------------------------------------------------------------------
-- | Check type signatures of term declarations.
checkDeclTermSig :: CheckDecl a
checkDeclTermSig _a ctx (DTerm (DeclTerm a n mpss tsResult mBody))
 = do   let wh  = [WhereTermDecl a n]
        mpss'     <- checkTermParamss a wh ctx mpss

        let ctx' =  foldl (flip contextBindTermParams) ctx mpss'
        tsResult' <- checkTypesAreAll a wh ctx' TData tsResult
        return  $ DTerm $ DeclTerm a n mpss' tsResult' mBody

checkDeclTermSig _ _ decl
 = return decl


-- | Check bodies of term declarations
checkDeclTerm :: CheckDecl a
checkDeclTerm _a ctx (DTerm (DeclTerm a n mpss tResult mBody))
 = do   let wh   = [WhereTermDecl a n]
        mpss'     <- checkTermParamss a wh ctx mpss
        let ctx' =  foldl (flip contextBindTermParams) ctx mpss'
        tsResult' <- checkTypesAreAll a wh ctx' TData tResult

        (mBody', _tsResult, _esResult)
         <- checkTerm a wh ctx' Synth mBody

        -- TODO: result type needs to be a vector.
        -- TODO: check against result type.
        -- TODO: check result type.
        -- TODO: check effects are empty.
        return  $ DTerm $ DeclTerm a n mpss' tsResult' mBody'

checkDeclTerm _ _ decl
 = return decl


---------------------------------------------------------------------------------------------------
-- | Check test declarations.
checkDeclTest :: CheckDecl a

-- (t-decl-kind) ------------------------------------------
checkDeclTest _a ctx (DTest (DeclTestKind a' n t))
 = do   let wh = [WhereTestKind a' n]
        (t', _k) <- checkType a' wh ctx t
        return  $ DTest $ DeclTestKind a' n t'


-- (t-decl-type) ------------------------------------------
checkDeclTest _a ctx (DTest (DeclTestType a' n m))
 = do   let wh  = [WhereTestType a' n]
        (m', _tResult, _esResult)
         <- checkTerm a' wh ctx Synth m
        return  $ DTest $ DeclTestType a' n m'


-- (t-decl-eval) ------------------------------------------
checkDeclTest _a ctx (DTest (DeclTestEval a' n m))
 = do   let wh  = [WhereTestEval a' n]
        (m', _tResult, _esResult)
         <- checkTerm a' wh ctx Synth m

        -- TODO: check effects are empty.
        return  $ DTest $ DeclTestEval a' n m'


-- (t-decl-exec) ------------------------------------------
checkDeclTest _a ctx (DTest (DeclTestExec a' n m))
 = do   let wh  = [WhereTestExec a' n]
        (m', _tResult, _esResult)
         <- checkTerm a' wh ctx Synth m

        -- TODO: check effects are empty.
        -- TODO: check expr returns a suspension
        return  $ DTest $ DeclTestExec a' n m'


-- (t-decl-assert) ----------------------------------------
checkDeclTest _a ctx (DTest (DeclTestAssert a' n m))
 = do   let wh  = [WhereTestAssert a' n]
        (m', _tResult, _esResult)
         <- checkTerm a' wh ctx (Check [TBool]) m

        -- TODO: check effects are empty.
        return  $ DTest $ DeclTestAssert a' n m'

checkDeclTest _a _ctx decl
 = return decl
