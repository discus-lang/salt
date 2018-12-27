
module Salt.Core.Exp.Module where
import Salt.Core.Exp.Type
import Salt.Core.Exp.Term
import Salt.Core.Exp.Name


---------------------------------------------------------------------------------------------------
-- | Modules.
data Module a
        = Module
        { moduleDecls           :: [Decl a] }
        deriving Show


-- | Top-level declarations in a module.
data Decl a
        = DType (DeclType a)
        | DTerm (DeclTerm a)
        | DTest (DeclTest a)
        deriving Show


-- | Type declaration
data DeclType a
        = DeclType
        { declAnnot             :: a
        , declName              :: Name
        , declParams            :: [TypeParams a]
        , declKindResult        :: Kind a
        , declBody              :: Type a }
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

        -- Execute an effectful term.
        | DeclTestExec
        { declAnnot             :: a
        , declTestName          :: Maybe Name
        , declTestBody          :: Term a }

        -- Check that a term evaluates to true.
        | DeclTestAssert
        { declAnnot             :: a
        , declTestName          :: Maybe Name
        , declTestBody          :: Term a }
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Get the annotation of a declaration.
annotOfDecl :: Decl a -> a
annotOfDecl decl
 = case decl of
        DType (DeclType         { declAnnot }) -> declAnnot
        DTerm (DeclTerm         { declAnnot }) -> declAnnot
        DTest (DeclTestKind     { declAnnot }) -> declAnnot
        DTest (DeclTestType     { declAnnot }) -> declAnnot
        DTest (DeclTestEval     { declAnnot }) -> declAnnot
        DTest (DeclTestExec     { declAnnot }) -> declAnnot
        DTest (DeclTestAssert   { declAnnot }) -> declAnnot


-- | Get the name of a declaration, if it has one.
nameOfDecl :: Decl a -> Maybe Name
nameOfDecl decl
 = case decl of
        DType (DeclType       { declName })     -> Just declName
        DTerm (DeclTerm       { declName })     -> Just declName
        DTest (DeclTestKind   { declTestName }) -> declTestName
        DTest (DeclTestType   { declTestName }) -> declTestName
        DTest (DeclTestEval   { declTestName }) -> declTestName
        DTest (DeclTestExec   { declTestName }) -> declTestName
        DTest (DeclTestAssert { declTestName }) -> declTestName


