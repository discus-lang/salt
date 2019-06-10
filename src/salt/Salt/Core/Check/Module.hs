
module Salt.Core.Check.Module where
import Salt.Core.Check.Module.DeclType
import Salt.Core.Check.Module.DeclTerm
import Salt.Core.Check.Module.DeclTest
import Salt.Core.Check.Module.DeclEmit
import Salt.Core.Check.Module.Base
import Salt.Core.Check.Type
import Salt.Core.Check.Term
import Salt.Core.Check.Term.Base
import qualified Data.Map       as Map
import qualified Data.Either    as Either


-- | Check a whole module.
---
--   We need to do this in stages to ensure that type synonyms are are well
--   kinded before checking terms that may mention them etc.
--
--   We want to do this so we don't accidently reduce type operator
--   applications that were actually ill-kinded. We also prefer to see errors
--   in the term declaratoins before dealing with errors in test declarations.
--
checkModule
        :: Annot a
        => a -> Module a
        -> IO (Either [Error a] (Module a, Context a))

checkModule a mm
 = goTypeSigs ctxStart (moduleDecls mm)
 where
        ctxStart
         = Context
         { contextOptions       = optionsDefault
         , contextCheckType     = checkTypeWith
         , contextSynthTerm     = synthTermWith
         , contextCheckTerm     = checkTermWith
         , contextModuleType    = Map.empty
         , contextModuleTerm    = Map.empty
         , contextLocal         = []
         , contextInside        = [] }

        -- Check kind signatures on types, before adding them to the context.
        goTypeSigs  ctx decls
         = do
                -- Check kind signatures on type decls.
                (decls', errsSig)
                 <- checkDecls (checkDeclTypeSig a ctx) decls

                -- Check type decls not rebound and not recursive.
                let errsRebound   = checkDeclTypeRebound   decls
                let errsRecursive = checkDeclTypeRecursive decls

                let errs = errsSig ++ errsRebound ++ errsRecursive
                if not $ null errs
                 then return $ Left errs
                 else do
                        let nktsDeclType
                                = [ ( n
                                  , ( makeKindOfDeclType pss kResult
                                    , makeTAbsOfParams pss tBody))
                                  | DType (DeclType _a n pss kResult tBody) <- decls' ]
                        let ctx' = ctx { contextModuleType = Map.fromList nktsDeclType }
                        goTypeDecls ctx' decls'

        -- Check individual type declarations.
        goTypeDecls ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclType a ctx) decls

                if not $ null errs
                 then return $ Left errs
                 else goTermSigs  ctx decls'

        -- Check type signatures on terms, before adding them to the context.
        goTermSigs ctx decls
         = do
                -- Check type signatures on term decls.
                (decls', errsSig)
                 <- checkDecls (checkDeclTermSig a ctx) decls

                -- Check term decls are not rebound,
                --  though they are permitted to be recursive.
                let errsRebound = checkDeclTermRebound decls

                let (errsDeclTerm, ntssDeclTerm)
                     = Either.partitionEithers $ map makeTypeOfDeclTerm decls'

                let errs = errsSig ++ errsRebound ++ errsDeclTerm
                let ntsDeclTerm = concat ntssDeclTerm

                if not $ null errs
                 then return $ Left errs
                 else do
                        let ctx' = ctx { contextModuleTerm = Map.fromList ntsDeclTerm }
                        goTermDecls ctx' decls'

        -- Check individual term declarations.
        goTermDecls ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclTerm a ctx) decls

                if not $ null errs
                 then return $ Left errs
                 else goTestSigs ctx decls'

        -- Check test signatures.
        goTestSigs ctx decls
         = do   let errs        = checkDeclTestRebound decls
                if not $ null errs
                 then return $ Left errs
                 else goTestDecls ctx decls

        -- Check individual test declarations.
        goTestDecls ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclTest a ctx) decls

                if not $ null errs
                 then return $ Left errs
                 else goEmitDecls ctx decls'

        -- Check individual emit declarations.
        goEmitDecls ctx decls
         = do   (decls', errs)
                 <- checkDecls (checkDeclEmit a ctx) decls

                if not $ null errs
                 then return $ Left errs
                 else return $ Right (mm { moduleDecls = decls' }, ctx)