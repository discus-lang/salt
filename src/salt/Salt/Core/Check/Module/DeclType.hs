
module Salt.Core.Check.Module.DeclType where
import Salt.Core.Check.Module.Base
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Params
import Salt.Core.Check.Type.Base
import qualified Salt.Core.Analysis.Support     as Support
import qualified Salt.Data.List                 as List

import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


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


-- | Check for rebound type declarations.
checkDeclTypeRebound :: Annot a => [Decl a] -> [Error a]
checkDeclTypeRebound decls
 = let  nsDeclType      = catMaybes [nameOfDecl d | d@DType{} <- decls]
        nsDup           = Set.fromList $ List.duplicates nsDeclType

        check (DType (DeclType aDecl nDecl _ _ _))
         | Set.member nDecl nsDup
         = Just $ ErrorTypeDeclRebound aDecl [WhereTypeDecl aDecl nDecl] nDecl
        check _ = Nothing

   in   mapMaybe check decls


-- | Check for recursive type declarations.
checkDeclTypeRecursive :: Annot a => [Decl a] -> [Error a]
checkDeclTypeRecursive decls
 = concatMap checkDecl decls
 where
        -- Check a single declaration.
        checkDecl decl
         = case decl of
                DType (DeclType aDecl nDecl _ _ _)
                  -> checkDeps aDecl nDecl Set.empty (Set.singleton nDecl)
                _ -> []

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

        -- Map of names of type decls to others they depend on.
        deps
         = Map.fromList
                [ (n, Support.freeTypeNamesOf tBody)
                | DType (DeclType _a n _ _ tBody) <- decls ]

        -- Get annotations for type declarations with the given name.
        -- This is used when reporting errors due to recursive declarations.
        nameAnnotsOfTypeDecl n
         = mapMaybe (nameAnnotOfTypeDecl n) decls

        nameAnnotOfTypeDecl n decl
         = case decl of
                DType (DeclType a n' _ _ _)
                 | n == n'      -> Just (n, a)
                _               -> Nothing

