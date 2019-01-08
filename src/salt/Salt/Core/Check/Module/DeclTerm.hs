
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
        tsResult' <- checkTypesAreAll UType a wh ctx' TData tsResult
        return  $ DTerm $ DeclTerm a n mpss' tsResult' mBody

checkDeclTermSig _ _ decl
 = return decl


-- | Check bodies of term declarations
checkDeclTerm :: CheckDecl a
checkDeclTerm _a ctx (DTerm (DeclTerm a nDecl mpss tsResult mBody))
 = do   let wh   = [WhereTermDecl a nDecl]

        -- Check the parameter type annotations.
        mpss'     <- checkTermParamss a wh ctx mpss

        -- Check the result type annotation.
        let ctx' =  foldl (flip contextBindTermParams) ctx mpss'
        tsResult' <- checkTypesAreAll UType a wh ctx' TData tsResult

        -- Check the body.
        (mBody', _tsResult, esResult)
         <- checkTerm a wh ctx' (Check tsResult) mBody

        -- The body must be pure.
        eBody_red <- simplType a ctx' (TSum esResult)
        when (not $ isTPure eBody_red)
         $ case reverse mpss of
            mps : _
             | Just _ <- takeMPTypes mps -> throw $ ErrorAbsImpure UType a wh eBody_red
             | Just _ <- takeMPTerms mps -> throw $ ErrorAbsImpure UTerm a wh eBody_red
            _ -> throw $ ErrorTermDeclImpure a wh nDecl eBody_red

        return  $ DTerm $ DeclTerm a nDecl mpss' tsResult' mBody'

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
        DTerm (DeclTerm a n pss0 tsResult _mBody)
         -> case loop tsResult pss0 of
                [t] -> Right [(n, t)]
                _   -> Left $ ErrorAbsTermNoValueForForall a
                                [ WhereTermDecl a n ] pss0
        DType{} -> Right []
        DTest{} -> Right []

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

