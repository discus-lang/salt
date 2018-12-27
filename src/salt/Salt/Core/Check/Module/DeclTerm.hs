
module Salt.Core.Check.Module.DeclTerm where
import Salt.Core.Check.Module.Base
import Salt.Core.Check.Term.Params
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base


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


