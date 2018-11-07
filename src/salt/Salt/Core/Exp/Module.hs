
module Salt.Core.Exp.Module where
import Salt.Core.Exp.Term
import Salt.Core.Exp.Name


-- | Modules.
data Module a
        = Module
        { moduleDecls           :: [Decl a] }
        deriving Show


-- | Top-level declarations in a module.
data Decl a
        = DTest  (DeclTest  a)
        deriving Show


-- | Test declaration.
data DeclTest a
        = DeclTestPrint
        { declAnnot             :: a
        , declTestName          :: Maybe Name
        , declTestBody          :: Term a }

        | DeclTestAssert
        { declAnnot             :: a
        , declTestName          :: Maybe Name
        , declTestBody          :: Term a }
        deriving Show

