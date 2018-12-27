
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


-- | Check for rebound term declarations.
checkDeclTermRebound :: Annot a => [Decl a] -> [Error a]
checkDeclTermRebound decls
 = let  nsDeclTerm = catMaybes [nameOfDecl d | d@DTerm{} <- decls]
        nsDup      = Set.fromList $ List.duplicates nsDeclTerm

        check (DTerm (DeclTerm aDecl nDecl _ _ _))
         | Set.member nDecl nsDup
         = Just $ ErrorTermDeclRebound aDecl [WhereTermDecl aDecl nDecl] nDecl
        check _ = Nothing

   in   mapMaybe check decls


---------------------------------------------------------------------------------------------------
-- | Make a type signature from the annotations on a term declaration.
--
--   This will fail if the term declaration is malformed so that it tries
--   to define a polymorphic binding that produces no value:
--
-- @ term thing @[a: #Data]: [] = []
-- @
--
--   We can't produce a type for this as the body of a forall must have
--   arity one. The syntax of types ensures this is always the case,
--   but we need to check for it explicitly in term bindings.
--
makeTypeOfDeclTerm :: Decl a -> Either (Error a) [(Name, Type a)]
makeTypeOfDeclTerm decl
 = case decl of
        DTerm (DeclTerm a n pss0 tsResult _mBody)
         -> case loop tsResult pss0 of
                [t] -> Right [(n, t)]
                _   -> Left $ ErrorAbsTermNoValueForForall a
                                [ WhereTermDecl a n ] pss0

        DType{} -> Right []
        DTest{} -> Right []

 where
        loop tsResult [] = tsResult

        loop tsResult (MPTerms bts : pss')
         = [TFun (map snd bts) (loop tsResult pss')]

        loop tsResult (MPTypes bts : pss')
         = case loop tsResult pss' of
                [t] -> [TForall bts t]
                _   -> []

