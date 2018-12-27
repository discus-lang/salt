
module Salt.Core.Check.Module where
import Salt.Core.Check.Module.DeclType
import Salt.Core.Check.Module.DeclTerm
import Salt.Core.Check.Module.DeclTest
import Salt.Core.Check.Module.Base
import Salt.Core.Check.Type
import Salt.Core.Check.Term
import Salt.Core.Check.Term.Base

import qualified Data.Map                       as Map


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

                -- Check type decls not rebound and not recursive.
                let errsRebound   = checkDeclTypeRebound   decls
                let errsRecursive = checkDeclTypeRecursive decls

                let errs    = errsSig ++ errsRebound ++ errsRecursive

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

