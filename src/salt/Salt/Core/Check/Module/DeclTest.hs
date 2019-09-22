
module Salt.Core.Check.Module.DeclTest where
import Salt.Core.Check.Module.Base
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import qualified Salt.Data.List as List
import qualified Data.Set       as Set


---------------------------------------------------------------------------------------------------
-- | Check test declarations.
checkDeclTest :: CheckDecl a

-- (t-decl-kind) ------------------------------------------
checkDeclTest _a ctx (DTest (DeclTestKind a' bWatch n t))
 = do   let wh    = [WhereTestDecl a' n]
        (t', _k) <- checkType a' wh ctx t
        return  $ DTest $ DeclTestKind a' bWatch n t'


-- (t-decl-type) ------------------------------------------
checkDeclTest _a ctx (DTest (DeclTestType a' bWatch n m))
 = do   let wh    = [WhereTestDecl a' n]
        (m', _tsResult, _esResult)
         <- synthTerm a' wh ctx m
        return  $ DTest $ DeclTestType a' bWatch n m'


-- (t-decl-eval-type) -------------------------------------
checkDeclTest _a ctx (DTest (DeclTestEvalType a bWatch nDecl t))
 = do   let wh  = [WhereTestDecl a nDecl]
        (t', _k) <- checkType a wh ctx t
        return  $ DTest $ DeclTestEvalType a bWatch nDecl t'


-- (t-decl-eval-term) -------------------------------------
checkDeclTest _a ctx (DTest (DeclTestEvalTerm a bWatch nDecl mBody))
 = do   let wh  = [WhereTestDecl a nDecl]

        -- Check the body term.
        (mBody', _tsResult, esResult)
         <- synthTerm a wh ctx mBody

        -- The body must be pure.
        eBody_simp <- simplType a ctx (TSum esResult)
        when (not $ isTPure eBody_simp)
         $ throw $ ErrorTestDeclImpure a wh nDecl eBody_simp

        return  $ DTest $ DeclTestEvalTerm a bWatch nDecl mBody'


-- (t-decl-exec) ------------------------------------------
checkDeclTest _a ctx (DTest (DeclTestExec a bWatch nDecl mBody))
 = do   let wh  = [WhereTestDecl a nDecl]

        -- Check the body term.
        (mBody', _tsResult, _esResult)
         <- synthTermProductive a wh ctx mBody

        return  $ DTest $ DeclTestExec a bWatch nDecl mBody'


-- (t-decl-assert) ----------------------------------------
checkDeclTest _a ctx (DTest (DeclTestAssert a bWatch nDecl mBody))
 = do   let wh  = [WhereTestDecl a nDecl]

        -- Check the body term.
        (mBody', _rr, esResult)
         <- checkTerm a wh ctx [TBool] mBody

        -- The body must be pure.
        eBody_simp <- simplType a ctx (TSum esResult)
        when (not $ isTPure eBody_simp)
         $ throw $ ErrorTestDeclImpure a wh nDecl eBody_simp

        return  $ DTest $ DeclTestAssert a bWatch nDecl mBody'

checkDeclTest _a _ctx decl
 = return decl


---------------------------------------------------------------------------------------------------
-- | Check for rebound test declarations.
checkDeclTestRebound :: Annot a => [Decl a] -> [Error a]
checkDeclTestRebound decls
 = let  nsDeclTerm = catMaybes [nameOfDecl d | d@DTest{} <- decls]
        nsDup      = Set.fromList $ List.duplicates nsDeclTerm

        check decl@(DTest _)
         | aDecl        <- annotOfDecl decl
         , Just nDecl   <- nameOfDecl  decl
         , Set.member nDecl nsDup
         = Just $ ErrorTestDeclRebound aDecl
                        [WhereTestDecl aDecl (Just nDecl)] nDecl
        check _ = Nothing

   in   mapMaybe check decls

