
module Salt.Core.Check.Module where
import Salt.Core.Check.Error
import Salt.Core.Check.Where
import Salt.Core.Check.Term
import Salt.Core.Check.Context
import Salt.Core.Exp
import qualified Control.Exception      as Control


---------------------------------------------------------------------------------------------------
-- | Check a whole module.
checkModule :: Annot a => a -> Module a -> IO (Module a, [Error a])
checkModule a mm
 = do
        -- Build the top level context.
        let ctx = contextEmpty

        (ds', errss)
         <- fmap unzip $ mapM (checkHandleDecl a ctx) $ moduleDecls mm

        return  ( mm { moduleDecls = ds' }
                , concat errss)


---------------------------------------------------------------------------------------------------
-- | Check a declaration and handle any errors that we find.
--   TODO: track a map of top level decls with type errors.
checkHandleDecl
        :: forall a. Annot a
        => a -> Context a -> Decl a -> IO (Decl a, [Error a])
checkHandleDecl a ctx decl
 = Control.try (checkDecl a ctx decl)
 >>= \case
        Right decl'             -> return (decl', [])
        Left (err :: Error a)   -> return (decl,  [err])


-- | Check the given declaration.
checkDecl :: Annot a => a -> Context a -> Decl a -> IO (Decl a)

-- TODO: check kind of type.
checkDecl _a _ctx (DTest (DeclTestKind a' n t))
 = do   return  $ DTest $ DeclTestKind a' n t

-- TODO: check type of term
checkDecl _a _ctx (DTest (DeclTestType a' n m))
 = do   return  $ DTest $ DeclTestType a' n m


checkDecl _a ctx (DTest (DeclTestEval a' n m))
 = do   (m', _tResult)
         <- checkTerm a' [WhereTestEval a' n] ctx m Synth

        return  $ DTest $ DeclTestEval a' n m'


checkDecl _a ctx (DTest (DeclTestAssert a' n m))
 = do   (m', _tResult)
         <- checkTerm a' [WhereTestAssert a' n] ctx m (Check [TBool])

        return  $ DTest $ DeclTestAssert a' n m'


