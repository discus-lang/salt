
module Salt.Core.Exp.Module where
import Salt.Core.Exp.Type
import Salt.Core.Exp.Term
import Salt.Core.Exp.Name


-- | Modules.
data Module a
        = Module
        { moduleDecls           :: [Decl a] }
        deriving Show


-- | Top-level declarations in a module.
data Decl a
        = DTest (DeclTest a)
        | DTerm (DeclTerm a)
        deriving Show


-- | Test declaration.
data DeclTest a
        -- Print the kind of a type.
        = DeclTestKind
        { declAnnot             :: a
        , declTestName          :: Maybe Name
        , declTestType          :: Type a }

        -- Print the type of a term.
        | DeclTestType
        { declAnnot             :: a
        , declTestName          :: Maybe Name
        , declTestTerm          :: Term a }

        -- Evaluate a term and print the result
        | DeclTestEval
        { declAnnot             :: a
        , declTestName          :: Maybe Name
        , declTestBody          :: Term a }

        -- Check that a term evaluates to true.
        | DeclTestAssert
        { declAnnot             :: a
        , declTestName          :: Maybe Name
        , declTestBody          :: Term a }
        deriving Show


-- | Term declaration
data DeclTerm a
        = DeclTerm
        { declAnnot             :: a
        , declName              :: Name
        , declParams            :: [TermParams a]
        , declTypesResult       :: [Type a]
        , declBody              :: Term a }
        deriving Show
