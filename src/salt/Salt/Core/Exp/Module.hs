
module Salt.Core.Exp.Module where
import Salt.Core.Transform.Ups
import Salt.Core.Exp.Ups
import Salt.Core.Exp.Type
import Salt.Core.Exp.Term
import Salt.Core.Exp.Name
import Data.Maybe
import qualified Data.Map       as Map

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


-- | Get the names of type declarations in a module.
typeNamesOfModule :: Module a -> [Name]
typeNamesOfModule mm
        = catMaybes [ nameOfDecl d | d@DType{} <- moduleDecls mm ]


-- | Get the names of term declarations in a module.
termNamesOfModule :: Module a -> [Name]
termNamesOfModule mm
        = catMaybes [ nameOfDecl d | d@DTerm{} <- moduleDecls mm ]


-- | Get the names of test declarations in a module.
testNamesOfModule :: Module a -> [Name]
testNamesOfModule mm
        = catMaybes [ nameOfDecl d | d@DTest{} <- moduleDecls mm ]


---------------------------------------------------------------------------------------------------
-- | Lookup a term from a module and local environment.
resolveTermBound
        :: Module a -> Env a -> Bound
        -> IO (Maybe (TermDef a))

resolveTermBound mm (Env bs0) (BoundWith n d0)
 = goEnv d0 upsEmpty upsEmpty bs0
 where
        -- Look through the local environment.
        goEnv d upsT upsM (EnvValues nvs : rest)
         | d < 0        = return Nothing
         | otherwise
         = let upsM' = upsCombine upsM (upsOfNames $ Map.keys nvs) in
           case Map.lookup n nvs of
                Nothing         -> goEnv d upsT upsM' rest
                Just v
                 | d == 0       -> return $ Just $ TermLocal (upsApplyValue upsT upsM' v)
                 | otherwise    -> goEnv (d - 1) upsT upsM' rest

        goEnv d upsT upsM (EnvTypes nts : rest)
         = let  upsT' = upsCombine upsT (upsOfNames $ Map.keys nts)
           in   goEnv d upsT' upsM rest

        goEnv d upsT upsM []
         | d == 0    = goGlobal upsT upsM
         | otherwise = return Nothing

        -- Look for declarations in the global context.
        goGlobal upsT upsM
         = let  decls  = moduleDecls mm
                psms   = [ (ps, tResult, mBody)
                         | DTerm (DeclTerm _a n' ps tResult mBody) <- decls
                         , n' == n ]

                -- We effectively have a recursive substitution at top level,
                -- so need to push our ups under it before applying the ups
                -- to the term we got from the binding.
                upsT'  = flip upsBumpNames upsT $ typeNamesOfModule mm
                upsM'  = flip upsBumpNames upsM $ termNamesOfModule mm

           in case psms of
                [(mpss, tResult, mBody)]
                  -> return $ Just $ TermDecl
                            $ upsApplyTerm upsT' upsM'
                            $ foldr MAbs (MThe tResult mBody) mpss

                _ -> return Nothing


-- | The definition mode of a resolved term.
data TermDef a
        = TermDecl   (Term  a)
        | TermLocal  (Value a)
        deriving Show


---------------------------------------------------------------------------------------------------
-- | The definition mode of a resolved type.
data TypeDef a
        -- | Type was defined as a global declaration.
        = TypeDecl   (Kind a) (Type a)

        -- | Type was defined in the local context,
        --   at the given level.
        | TypeLocal  (Kind a) Int

        -- | Type was defined in a local parameter at the given level,
        --   and is subject to alpha-conversion.
        | TypeParam  (Kind a) Int
        deriving Show

