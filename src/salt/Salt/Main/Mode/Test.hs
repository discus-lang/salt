
module Salt.Main.Mode.Test where
import Salt.Main.Mode.Check
import Salt.Main.Mode.Parse
import Salt.Core.Exp
import Salt.Data.Pretty
import Salt.Data.Location
import qualified Salt.Core.Eval         as Eval
import qualified Salt.Core.Check        as Check
import qualified Salt.Data.Pretty       as P

import qualified System.IO              as System
import qualified Data.Text              as T


------------------------------------------------------------------------------------------- Main --
-- | Run all the tests in the given source file,
--   printing the result to stdout.
mainTests :: FilePath -> IO ()
mainTests filePath
 = do   mm         <- runParse filePath
        (mm', ctx) <- runCheck filePath mm

        let tests = [ d | DTest d <- moduleDecls mm' ]
        mapM_ (runTest ctx mm') tests


-- | Run a single test in the given source file,
--   printing the result to stdout.
mainTest :: FilePath -> Text -> IO ()
mainTest filePath name
 = do
        mm         <- runParse filePath
        (mm', ctx) <- runCheck filePath mm

        let tests = [ d | DTest d <- moduleDecls mm'
                        , declTestName d == Just (Name name) ]

        case tests of
         []     -> error $ "mainTest: no test named: " ++ show name
         _      -> mapM_ (runTest ctx mm') tests


-- | Run a single test declaration from a type-checked module.
runTest :: Check.Context RL -> Module RL -> DeclTest RL -> IO ()
runTest ctx mm tt
 = case tt of
        DeclTestKind     _ _ n t -> runTestKind     ctx mm n t
        DeclTestType     _ _ n m -> runTestType     ctx mm n m
        DeclTestEvalType _ _ n t -> runTestEvalType mm n t
        DeclTestEvalTerm _ _ n m -> runTestEvalTerm mm n m
        DeclTestExec     _ _ n m -> runTestExec     mm n m
        DeclTestAssert   _ _ n m -> runTestAssert   mm n m


--------------------------------------------------------------------------------------- TestKind --
-- | Run a kind test.
--   We kind-check a type and print the result kind.
runTestKind
        :: Check.Context RL -> Module RL
        -> Maybe Name -> Type RL -> IO ()

runTestKind ctx _mm mnTest tTest
 = do
        -- Print the test name.
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        -- Check the type in an empty environment and print he result.
        (_t, kResult) <- Check.checkType rlNone [] ctx tTest
        putStrLn $ P.render $ P.ppr () kResult


--------------------------------------------------------------------------------------- TestType --
-- | Run a type test.
--   We type-check a term and print the result type.
runTestType
        :: Check.Context RL -> Module RL
        -> Maybe Name -> Term RL -> IO ()

runTestType ctx _mm mnTest mTest
 = do
        -- Print the test name.
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        -- Check the term in an empty environment and print the result.
        (_m, tsResult, _esResult)
         <- Check.synthTermProductive rlNone [] ctx mTest
        case tsResult of
         [t]    -> putStrLn $ P.render $ P.ppr () t
         _      -> putStrLn $ P.render $ P.ppr () tsResult


----------------------------------------------------------------------------------- TestEvalType --
-- | Run a type eval test.
runTestEvalType
        :: Module RL
        -> Maybe Name -> Type RL -> IO ()

runTestEvalType mm mnTest tTest
 = do
        -- Print the test name.
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        -- Initialize the machine state.
        state   <- Eval.newState Eval.configDefault mm

        -- Reduce the type in an empty environment and print the result.
        tResult <- Eval.evalType state rlNone (TypeEnv []) tTest
        putStrLn $ P.render $ P.ppr () tResult


----------------------------------------------------------------------------------- TestEvalTerm --
-- | Run a term eval test.
runTestEvalTerm
        :: Module RL
        -> Maybe Name -> Term RL -> IO ()

runTestEvalTerm mm mnTest mTest
 = do
        -- Print the test name.
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        -- Initialize the machine state.
        state   <- Eval.newState Eval.configDefault mm

        -- Evaluate the term in an empty environment and print the result.
        vResult <- Eval.evalTerm state rlNone (TermEnv []) mTest
        putStrLn $ P.render $ P.ppr () vResult


--------------------------------------------------------------------------------------- TestExec --
-- | Run a exec test.
--   We evaluate the term to a suspension then run it.
runTestExec
        :: Module RL
        -> Maybe Name -> Term RL -> IO ()

runTestExec mm mnTest mTest
 = do
        -- Print the test name.
        (case mnTest of
                Nothing -> return ()
                Just (Name tx) -> do
                        putStr $ T.unpack tx % ": "
                        System.hFlush System.stdout)

        -- Initialize the machine state.
        state   <- Eval.newState Eval.configDefault mm

        -- Evaluate the term in an empty environment and print the result.
        vsSusp  <- Eval.evalTerm state rlNone (TermEnv []) mTest
        case vsSusp of
                [VClosure (TermClosure env (MPTerms []) mBody)]
                 -> do  vsResult <- Eval.evalTerm state rlNone env mBody
                        case vsResult of
                         [] -> return ()
                         _  -> putStrLn $ P.render $ P.ppr () vsResult

                [] -> return ()
                _  -> putStrLn $ P.render $ P.ppr () vsSusp


------------------------------------------------------------------------------------- TestAssert --
-- | Run an assert test.
--   We evaluate the term and check that the result value is #true.
runTestAssert
        :: Module RL
        -> Maybe Name -> Term RL -> IO ()

runTestAssert mm mnTest mTest
 = do
        -- Print the test name.
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        -- Initialize the machine state.
        state   <- Eval.newState Eval.configDefault mm

        -- Evaluate the term in an empty environment,
        --   and print the test result based on the boolean return value.
        vResult <- Eval.evalTerm state rlNone (TermEnv []) mTest
        case vResult of
         [VTrue]  -> putStrLn "ok"
         [VFalse] -> putStrLn "failed"
         _        -> putStrLn "invalid"

