
module Salt.Core.Check.Module.DeclTerm where
import Salt.Core.Check.Module.Base
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import qualified Salt.Data.List as List
import qualified Data.Set       as Set


---------------------------------------------------------------------------------------------------
-- | Check type signatures of term declarations.
checkDeclTermSig :: CheckDecl a
checkDeclTermSig _a ctx (DTerm (DeclTerm a mode n mpss tsResult mBody))
 = do   let wh  = [WhereTermDecl a n]
        (ctx', mpss') <- checkTermParamss a wh ctx mpss
        tsResult'     <- checkTypesAreAll UType a wh ctx' TRepr tsResult
        return  $ DTerm $ DeclTerm a mode n mpss' tsResult' mBody

checkDeclTermSig _ _ decl
 = return decl


-- | Check bodies of term declarations
checkDeclTerm :: CheckDecl a

-- (d-term-plain) -----------------------------------------
checkDeclTerm _a ctx
        (DTerm (DeclTerm a mode@DeclTermModePlain nDecl mpss tsResult mBody))
 = do
        let wh  = [WhereTermDecl a nDecl]

        -- Check the parameter type annotations.
        (ctx', mpss') <- checkTermParamss a wh ctx mpss

        -- Check the result type annotation.
        tsResult' <- checkTypesAreAll UType a wh ctx' TRepr tsResult

        -- Check the body.
        (mBody', _rr, esResult)
         <- checkTerm a wh ctx' tsResult mBody

        -- The body must be pure.
        eBody_red <- simplType a ctx' (TSum esResult)
        when (not $ isTPure eBody_red)
         $ case reverse mpss of
            mps : _
             | Just _ <- takeMPTypes mps -> throw $ ErrorAbsImpure UType a wh eBody_red
             | Just _ <- takeMPTerms mps -> throw $ ErrorAbsImpure UTerm a wh eBody_red
            _ -> throw $ ErrorTermDeclImpure a wh nDecl eBody_red

        return  $ DTerm $ DeclTerm a mode nDecl mpss' tsResult' mBody'

-- (d-term-proc) ------------------------------------------
checkDeclTerm _a ctx
        (DTerm (DeclTerm a mode@DeclTermModeProc nDecl mpss tsResult0 mBody))
 = do
        let wh  = [WhereTermDecl a nDecl]

        -- Check the parameter type annotations.
        (ctx', mpss') <- checkTermParamss a wh ctx mpss

        -- Check the result type annotation.
        (tsResult, esResult)
         <- simplTypes a ctx' tsResult0
         >>= \case
                [TSusp tsResult tEffect]
                  -> return (tsResult, [tEffect])
                t -> return (t, [])

        tsResult' <- checkTypesAreAll UType a wh ctx' TRepr tsResult

        -- Check the body.
        (mBody', _rr, esResult')
         <- contextCheckTerm ctx a wh
                ctx' { contextInside = [InsideLaunch tsResult']}
                tsResult
                mBody

        -- Check result effects against the annotation.
        eActual   <- simplType a ctx' $ TSum esResult'
        eExpected <- simplType a ctx' $ TSum esResult
        mErr <- checkTypeEquiv ctx a [] eExpected a [] eActual
        case mErr of
         Just _  -> throw $ ErrorMismatch UType a wh eActual eExpected
         Nothing -> return $ DTerm $ DeclTerm a mode nDecl mpss' tsResult' mBody'

checkDeclTerm _ _ decl
 = return decl


-- | Check for rebound term declarations.
checkDeclTermRebound :: Annot a => [Decl a] -> [Error a]
checkDeclTermRebound decls
 = let  nsDeclTerm = catMaybes [nameOfDecl d | d@DTerm{} <- decls]
        nsDup      = Set.fromList $ List.duplicates nsDeclTerm

        check (DTerm (DeclTerm aDecl _mode nDecl _ _ _))
         | Set.member nDecl nsDup
         = Just $ ErrorTermDeclRebound aDecl [WhereTermDecl aDecl nDecl] nDecl
        check _ = Nothing

   in   mapMaybe check decls


---------------------------------------------------------------------------------------------------
-- | Make a type signature from the annotations on a term declaration.
--
--   This will fail if the term declaration tries to define a polymorphic
--   binding that produces no value, for example:
--
-- @ term thing @[a: #Data]: [] = []
-- @
--
--   We can't produce a type for this as the body of a forall must have arity
--   one. The concrete syntax of types ensures this is always the case for
--   types that appear in the source program, but we need to check for it
--   explicitly when constructing types from term declarations.
--
makeTypeOfDeclTerm :: Decl a -> Either (Error a) [(Name, Type a)]
makeTypeOfDeclTerm decl
 = case decl of
        DTerm (DeclTerm a mode n pss0 tsResult _mBody)
         -- A procedure declaration must have at least an empty parameter list,
         -- otherwise it is a CAF rather than representing a block of code
         -- that we can execute.
         | []           <- pss0
         , DeclTermModeProc <- mode
         -> Left $ ErrorTermDeclProcNoParams a [WhereTermDecl a n] n

         -- Term declaration has no parameters, but binds a single term.
         -- This is a CAF, which is fine.
         | []           <- pss0
         , [tResult]    <- tsResult
         -> Right [(n, tResult)]

         -- Term declaration has no parameters or result types,
         -- so doesn't really declare anything.
         | []           <- pss0
         , []           <- tsResult
         -> Left $ ErrorTermDeclEmpty a [WhereTermDecl a n] n

         -- Build the functional type for the declaration.
         |  [tResult]   <- loop tsResult pss0
         -> Right [(n, tResult)]

         -- We have a binding with type parameters but no term paramters,
         -- so can't build a well kinded forall type for it.
         | otherwise
         -> Left $ ErrorAbsTermNoValueForForall a [WhereTermDecl a n] pss0

        DType{} -> Right []
        DTest{} -> Right []

        DEmit{} -> Right []

 where
        loop tsResult [] = tsResult

        loop tsResult (MPAnn _ mps' : pss')
         = loop tsResult (mps' : pss')

        loop tsResult (MPTerms bts : pss')
         = [TFun (map snd bts) (loop tsResult pss')]

        loop tsResult (MPTypes bts : pss')
         = case loop tsResult pss' of
                [t] -> [TForall (TPTypes bts) t]
                _   -> []

