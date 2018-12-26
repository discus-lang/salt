
module Salt.Core.Check.Module where
import Salt.Core.Check.Error
import Salt.Core.Check.Where
import Salt.Core.Check.Type
import Salt.Core.Check.Term
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Params
import Salt.Core.Check.Type.Base
import qualified Control.Exception      as Control
import qualified Data.Map               as Map


---------------------------------------------------------------------------------------------------
-- | Check a whole module.
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
         = do   -- TODO: just check the sig part.
                (decls', errs)
                 <- checkDecls (checkDeclTypeSig a ctx) decls

                let nktsDeclType
                        = [ ( n
                            , ( makeDeclKindOfParamsResult pss kResult
                              , makeTAbsOfParams pss tBody))
                          | DType (DeclType _a n pss kResult tBody) <- decls' ]

                let ctx' = ctx { contextModuleType = Map.fromList nktsDeclType }
                if null errs
                 then goTypeDecls ctx' decls'
                 else return (ctx, mm, errs)

        -- Check type declarations.
        goTypeDecls ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclType a ctx) decls

                if null errs
                 then goTermSigs  ctx decls'
                 else return (ctx, mm, errs)

        -- Check type signatures on terms, before adding them to the context.
        goTermSigs ctx decls
         = do   -- TODO: check just the sig part.
                (decls', errs)
                 <- checkDecls (checkDeclTermSig a ctx) decls

                -- Extract a list of type signatures for top-level declarations.
                -- TODO: kind-check these before adding them to the context.
                let ntsDeclTerm
                        = [ (n, makeDeclTypeOfParamsResult pss tsResult)
                          | DTerm (DeclTerm _a n pss tsResult _mBody) <- decls' ]

                let ctx' = ctx { contextModuleTerm = Map.fromList ntsDeclTerm }
                if null errs
                 then goTermDecls ctx' decls'
                 else return (ctx, mm, errs)

        goTermDecls ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclTerm a ctx) decls

                if null errs
                 then goTestDecls ctx decls'
                 else return (ctx, mm, errs)

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
checkDeclTypeSig _a _ctx decl@(DType _)
 = return decl

checkDeclTypeSig _ _ decl
 = return decl


-- | Check bodies of type declarations.
checkDeclType :: CheckDecl a
checkDeclType _a ctx (DType (DeclType a n tpss kResult tBody))
 = do   let wh   = [WhereTypeDecl a n]
        tpss'    <- checkTypeParamss a wh ctx tpss
        let ctx' =  foldl (flip contextBindTypeParams) ctx tpss'
        tBody'   <- checkTypeIs a wh ctx' kResult tBody
        return  $ DType $ DeclType a n tpss' kResult tBody'

checkDeclType _ _ decl
 = return decl


---------------------------------------------------------------------------------------------------
-- | Check type signatures of term declarations.
checkDeclTermSig :: CheckDecl a
checkDeclTermSig _a _ctx decl@(DTerm _)
 = return decl

checkDeclTermSig _ _ decl
 = return decl


-- | Check bodies of term declarations
checkDeclTerm :: CheckDecl a
checkDeclTerm _a ctx (DTerm (DeclTerm a n mpss mtResult mBody))
 = do   let wh   = [WhereTermDecl a n]
        mpss'    <- checkTermParamss a wh ctx mpss
        let ctx' =  foldl (flip contextBindTermParams) ctx mpss'

        (mBody', _tsResult, _esResult)
         <- checkTerm a wh ctx' Synth mBody

        -- TODO: result type needs to be a vector.
        -- TODO: check against result type.
        -- TODO: check result type.
        -- TODO: check effects are empty.
        return  $ DTerm $ DeclTerm a n mpss' mtResult mBody'

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
