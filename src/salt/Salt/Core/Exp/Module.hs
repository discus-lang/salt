
module Salt.Core.Exp.Module where
import Salt.Core.Transform.Ups
import Salt.Core.Exp.Ups
import Salt.Core.Exp.Type
import Salt.Core.Exp.Term
import Salt.Core.Exp.Name
import Data.Maybe
import qualified Data.Map               as Map


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
        | DEmit (DeclEmit a)
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
        , declTermMode          :: DeclTermMode
        , declName              :: Name
        , declParams            :: [TermParams a]
        , declTypesResult       :: [Type a]
        , declBody              :: Term a }
        deriving Show


-- | Mode of a decl term.
data DeclTermMode
        = DeclTermModePlain
        | DeclTermModeProc
        deriving (Show, Eq)


-- | Test declaration.
data DeclTest a
        -- Print the kind of a type.
        = DeclTestKind
        { declAnnot             :: a
        , declTestWatch         :: Bool
        , declTestName          :: Maybe Name
        , declTestType          :: Type a }

        -- Print the type of a term.
        | DeclTestType
        { declAnnot             :: a
        , declTestWatch         :: Bool
        , declTestName          :: Maybe Name
        , declTestTerm          :: Term a }

        -- Evaluate a type and print the result
        | DeclTestEvalType
        { declAnnot             :: a
        , declTestWatch         :: Bool
        , declTestName          :: Maybe Name
        , declTestType          :: Type a }

        -- Evaluate a term and print the result
        | DeclTestEvalTerm
        { declAnnot             :: a
        , declTestWatch         :: Bool
        , declTestName          :: Maybe Name
        , declTestTerm          :: Term a }

        -- Execute an effectful term.
        | DeclTestExec
        { declAnnot             :: a
        , declTestWatch         :: Bool
        , declTestName          :: Maybe Name
        , declTestBody          :: Term a }

        -- Check that a term evaluates to true.
        | DeclTestAssert
        { declAnnot             :: a
        , declTestWatch         :: Bool
        , declTestName          :: Maybe Name
        , declTestBody          :: Term a }
        deriving Show


-- | Emit declaration.
data DeclEmit a
        -- Emit an object.
        = DeclEmit
        { declAnnot             :: a
        , declEmitName          :: Maybe Name
        , declEmitBody          :: Term a }
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
        DTest (DeclTestEvalType { declAnnot }) -> declAnnot
        DTest (DeclTestEvalTerm { declAnnot }) -> declAnnot
        DTest (DeclTestExec     { declAnnot }) -> declAnnot
        DTest (DeclTestAssert   { declAnnot }) -> declAnnot
        DEmit (DeclEmit         { declAnnot }) -> declAnnot


-- | Get the name of a declaration, if it has one.
nameOfDecl :: Decl a -> Maybe Name
nameOfDecl decl
 = case decl of
        DType (DeclType         { declName })     -> Just declName
        DTerm (DeclTerm         { declName })     -> Just declName
        DTest (DeclTestKind     { declTestName }) -> declTestName
        DTest (DeclTestType     { declTestName }) -> declTestName
        DTest (DeclTestEvalType { declTestName }) -> declTestName
        DTest (DeclTestEvalTerm { declTestName }) -> declTestName
        DTest (DeclTestExec     { declTestName }) -> declTestName
        DTest (DeclTestAssert   { declTestName }) -> declTestName
        DEmit (DeclEmit         { declEmitName }) -> declEmitName


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


-- | Lookup a `DeclType` from a module.
--
--   If the declaration is multiply bound then `Nothing`.
lookupDeclTypeOfModule :: Name -> Module a -> Maybe (DeclType a)
lookupDeclTypeOfModule n mm
 = case [d | DType d@(DeclType _ n' _ _ _) <- moduleDecls mm
           , n == n']
   of   d : _   -> Just d
        _       -> Nothing


-- | Lookup a `DeclTerm` from a module.
--
--   If the declaration is multiply bound then `Nothing`.
--
lookupDeclTermOfModule :: Name -> Module a -> Maybe (DeclTerm a)
lookupDeclTermOfModule n mm
 = case [d | DTerm d@(DeclTerm _ _ n' _ _ _) <- moduleDecls mm
           , n == n']
   of   d : _   -> Just d
        _       -> Nothing


---------------------------------------------------------------------------------------------------
-- Lookup a type from a module and local environment.
resolveTypeBound
        :: Module a -> TypeEnv a -> Bound
        -> IO (Maybe (TypeDef a))

resolveTypeBound mm (TypeEnv bs0) (BoundWith n d0)
 = goEnv d0 upsEmpty bs0
 where
        -- Look through the local environment.
        goEnv d upsT (TypeEnvTypes nts : rest)
         | d < 0     = return Nothing
         | otherwise
         = let upsT' = upsCombine upsT (upsOfNames $ Map.keys nts) in
           case Map.lookup n nts of
                Nothing      -> goEnv d upsT' rest
                Just t
                 | d == 0    -> return $ Just $ TypeDefLocal (upsApplyType upsT' t)
                 | otherwise -> goEnv (d - 1) upsT' rest

        goEnv d upsT []
         | d == 0    = goGlobal upsT
         | otherwise = return Nothing

        -- Look for declarations in the global context.
        --   We effectively have a recursive substitution at top level,
        --   so need to push our ups under it before applying the ups
        --   to the term we got from the binding.
        goGlobal upsT
         = let upsT' = flip upsBumpNames upsT $ typeNamesOfModule mm
           in case lookupDeclTypeOfModule n mm of
                Just (DeclType _a _n tpss _kResult tBody)
                  -> return $ Just $ TypeDefDecl
                            $ upsApplyType upsT'
                            $ foldr TAbs tBody tpss

                _ -> return Nothing


-- | The definition mode of a resolved term.
data TypeDef a
        = TypeDefDecl   (Type a)
        | TypeDefLocal  (Type a)
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Lookup a term from a module and local environment.
resolveTermBound
        :: Show a => Module a -> TermEnv a -> Bound
        -> IO (Maybe (TermDef a))

resolveTermBound mm (TermEnv bs0) (BoundWith n d0)
 = goEnv d0 upsEmpty upsEmpty bs0
 where
        -- Look through the local environment.
        goEnv d upsT upsM (TermEnvTypes nts : rest)
         = let  upsT' = upsCombine upsT (upsOfNames $ Map.keys nts)
           in   goEnv d upsT' upsM rest

        goEnv d upsT upsM (TermEnvValues nvs : rest)
         | d < 0     = return Nothing
         | otherwise
         = let upsM' = upsCombine upsM (upsOfNames $ Map.keys nvs) in
           case Map.lookup n nvs of
                Nothing      -> goEnv d upsT upsM' rest
                Just v
                 | d /= 0    -> goEnv (d - 1) upsT upsM' rest
                 | otherwise
                 -> return $ Just $ TermDefLocal
                           $ upsApplyValue upsT upsM' v

        goEnv d upsT upsM (tvb@(TermEnvValuesRec ncs) : rest)
         | d < 0    = return Nothing
         | otherwise
         = let  upsM' = upsCombine upsM (upsOfNames $ Map.keys ncs) in
           case Map.lookup n ncs of
                Nothing      -> goEnv d upsT upsM' rest
                Just (TermClosure (TermEnv tvbs) mps m)
                 | d /= 0 -> goEnv (d - 1) upsT upsM' rest
                 | otherwise
                 -> return $ Just $ TermDefLocal
                           $ upsApplyValue upsT upsM
                           $ VClosure $ TermClosure (TermEnv (tvb : tvbs)) mps m

        goEnv d upsT upsM []
         | d == 0    = goGlobal upsT upsM
         | otherwise = return Nothing

        -- Look for declarations in the global context.
        --   We effectively have a recursive substitution at top level,
        --   so need to push our ups under it before applying the ups
        --   to the term we got from the binding.
        goGlobal upsT upsM
         = let  upsT' = flip upsBumpNames upsT $ typeNamesOfModule mm
                upsM' = flip upsBumpNames upsM $ termNamesOfModule mm
           in case lookupDeclTermOfModule n mm of
                -- For plain declarations we inject the result type
                -- using a 'the' construct.
                Just (DeclTerm _a DeclTermModePlain _n mpss tResult mBody)
                  -> return $ Just $ TermDefDecl
                            $ upsApplyTerm upsT' upsM'
                            $ foldr MAbs (MThe tResult mBody) mpss

                -- For proc declarations we also inject a 'launch'
                -- construct so that the body can return from it.
                -- TODO: we'll need to split out the boxings for the launch
                -- to be well typed here.
                Just (DeclTerm _a DeclTermModeProc _n mpss tsResult mBody)
                  -> return $ Just $ TermDefDecl
                            $ upsApplyTerm upsT' upsM'
                            $ foldr MAbs (MThe tsResult (MLaunch tsResult mBody)) mpss

                _ -> return Nothing


-- | The definition mode of a resolved term.
data TermDef a
        = TermDefDecl   (Term  a)
        | TermDefLocal  (Value a)
        deriving Show


