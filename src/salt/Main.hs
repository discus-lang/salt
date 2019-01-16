
module Main where
import Salt.Main.Config
import Salt.Core.Exp
import Salt.Data.Pretty
import Salt.Data.Location
import qualified Salt.LSP.Driver                as LSP
import qualified Salt.Core.Eval                 as Eval
import qualified Salt.Core.Check                as Check
import qualified Salt.Core.Check.Error          as Error
import qualified Salt.Core.Check.Where          as Where
import qualified Salt.Core.Codec.Text.Lexer     as Lexer
import qualified Salt.Core.Codec.Text.Token     as Token
import qualified Salt.Core.Codec.Text.Parser    as Parser
import qualified Salt.Data.Pretty               as P

import qualified Text.Lexer.Inchworm.Char       as IW

import Data.Function
import qualified System.Environment             as System
import qualified System.Exit                    as System
import qualified System.IO                      as System
import qualified Text.Show.Pretty               as Show
import qualified Data.Text                      as T


-- Main -------------------------------------------------------------------------------------------
main
 = do   args    <- System.getArgs
        config  <- parseArgs args configDefault
        case configMode config of
         Just (ModeLex   filePath)      -> mainLex    filePath
         Just (ModeParse filePath)      -> mainParse  filePath
         Just (ModeCheck filePath)      -> mainCheck  filePath
         Just (ModeTest  filePath)      -> mainTests  filePath
         Just (ModeTest1 filePath name) -> mainTest   filePath name
         Just (ModeLSP   mFileLog)      -> LSP.runLSP mFileLog
         _ -> do  putStr usage
                  System.exitFailure

type RL = Range Location
rlNone  = Range (Location 0 0) (Location 0 0)


-- Lex --------------------------------------------------------------------------------------------
mainLex filePath
 = do   toks       <- runLex filePath
        putStr  $ unlines $ map show toks

runLex filePath
 = do   source  <- readFile filePath

        (toks, loc, strRest)
         <- IW.scanStringIO source Lexer.scanner

        let toks'  = [ Token.At l k
                     | Token.At l k <- toks
                     , k & \case Token.KComment _ -> False
                                 _                -> True]

        case strRest of
         [] -> return toks'
         _
          -> System.die
                $ "lexical error at "
                ++ show loc
                ++ " " ++ show (take 10 strRest) ++ "..."


-- Parse ------------------------------------------------------------------------------------------
mainParse :: FilePath -> IO ()
mainParse filePath
 = do   mm      <- runParse filePath
        putStrLn $ Show.ppShow mm


runParse  :: FilePath -> IO (Module RL)
runParse filePath
 = do   toks       <- runLex filePath
        let result = Parser.parseModule toks

        case result of
         Left errs
          -> do putStrLn $ P.render $ vcat
                         $ map (Parser.ppParseError filePath) errs
                System.exitFailure

         Right mm -> return mm


-- Check ------------------------------------------------------------------------------------------
mainCheck :: FilePath -> IO ()
mainCheck filePath
 = do   mm      <- runParse filePath
        runCheck filePath mm
        return ()

runCheck :: FilePath -> Module RL -> IO (Check.Context RL)
runCheck filePath mm
 = do   Check.checkModule rlNone mm
         >>= \case
                Right (_mm', ctx) -> return ctx
                Left errs
                 -> do  mapM_ (printError filePath) errs
                        System.exitFailure

printError filePath err
 = do   let (Range (Location nLine nCol) _)
                = Error.errorAnnot err
        putStrLn
         $ P.render $ vcat
         $      [ P.padL 6 $ P.string filePath
                        % text ":" % P.string (show (nLine + 1))
                        % text ":" % P.string (show (nCol + 1))
                , P.indent 2 $ ppr () err ]

         ++ [ empty]
         ++ [ let (Range (Location nLine' nCol') _)
                    = Where.whereAnnot wh
              in  P.indent 2
                        $  P.padL 6
                                ( P.string (show (nLine' + 1))
                                % text ":" % (P.string (show (nCol' + 1))))
                        %% P.ppr () wh
                | wh <- Error.errorWhere err ]

         ++ [P.empty]


-- Test -------------------------------------------------------------------------------------------
mainTest :: FilePath -> Text -> IO ()
mainTest filePath name
 = do
        mm      <- runParse filePath
        ctx     <- runCheck filePath mm

        let tests = [ d | DTest d <- moduleDecls mm
                        , declTestName d == Just (Name name) ]

        case tests of
         []     -> error $ "mainTest: no test named: " ++ show name
         _      -> mapM_ (runTest ctx mm) tests


mainTests :: FilePath -> IO ()
mainTests filePath
 = do   mm      <- runParse filePath
        ctx     <- runCheck filePath mm

        let tests = [ d | DTest d <- moduleDecls mm ]
        mapM_ (runTest ctx mm) tests


runTest :: Check.Context RL -> Module RL -> DeclTest RL -> IO ()
runTest ctx mm tt
 = case tt of
        DeclTestKind     _ _ n t -> runTestKind     ctx mm n t
        DeclTestType     _ _ n m -> runTestType     ctx mm n m
        DeclTestEvalType _ _ n t -> runTestEvalType mm n t
        DeclTestEvalTerm _ _ n m -> runTestEvalTerm mm n m
        DeclTestExec     _ _ n m -> runTestExec     mm n m
        DeclTestAssert   _ _ n m -> runTestAssert   mm n m


-- TestKind ---------------------------------------------------------------------------------------
-- | Run a kind test.
--   We kind-check a type and print the result kind.
runTestKind
        :: Check.Context RL -> Module RL
        -> Maybe Name -> Type RL -> IO ()

runTestKind ctx _mm mnTest tTest
 = do
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        (_t, kResult) <- Check.checkType rlNone [] ctx tTest
        putStrLn $ P.render $ P.ppr () kResult


-- TestType ---------------------------------------------------------------------------------------
-- | Run a type test.
--   We type-check a term and print the result type.
runTestType
        :: Check.Context RL -> Module RL
        -> Maybe Name -> Term RL -> IO ()

runTestType ctx _mm mnTest mTest
 = do
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        (_m, tsResult, _esResult)
         <- Check.checkTerm rlNone [] ctx Check.Synth mTest
        case tsResult of
         [t]    -> putStrLn $ P.render $ P.ppr () t
         _      -> putStrLn $ P.render $ P.ppr () tsResult


-- TestEvalType -----------------------------------------------------------------------------------
-- | Run a type eval test.
runTestEvalType
        :: Module RL
        -> Maybe Name -> Type RL -> IO ()

runTestEvalType mm mnTest tTest
 = do
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        let state
                = Eval.State
                { Eval.stateConfig      = Eval.configDefault
                , Eval.stateModule      = mm }

        tResult <- Eval.evalType state rlNone (TypeEnv []) tTest
        putStrLn $ P.render $ P.ppr () tResult


-- TestEvalTerm -----------------------------------------------------------------------------------
-- | Run a term eval test.
runTestEvalTerm
        :: Module RL
        -> Maybe Name -> Term RL -> IO ()

runTestEvalTerm mm mnTest mTest
 = do
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        let state
                = Eval.State
                { Eval.stateConfig      = Eval.configDefault
                , Eval.stateModule      = mm }

        vResult <- Eval.evalTerm state rlNone (TermEnv []) mTest
        putStrLn $ P.render $ P.ppr () vResult


-- TestExec ---------------------------------------------------------------------------------------
-- | Run a exec test.
--   We evaluate the term to a suspension then run it.
runTestExec
        :: Module RL
        -> Maybe Name -> Term RL -> IO ()

runTestExec mm mnTest mTest
 = do
        (case mnTest of
                Nothing -> return ()
                Just (Name tx) -> do
                        putStr $ T.unpack tx % ": "
                        System.hFlush System.stdout)

        let state
                = Eval.State
                { Eval.stateConfig      = Eval.configDefault
                , Eval.stateModule      = mm }

        vsSusp <- Eval.evalTerm state rlNone (TermEnv []) mTest

        case vsSusp of
                [VClosure (TermClosure (TermEnv []) (MPTerms []) mBody)]
                 -> do  vsResult <- Eval.evalTerm state rlNone (TermEnv []) mBody
                        case vsResult of
                         [] -> return ()
                         _  -> putStrLn $ P.render $ P.ppr () vsResult

                _ -> error $ "runTestEval: term did not produce a suspension"
                           ++ show vsSusp


-- TestAssert -------------------------------------------------------------------------------------
-- | Run an assert test.
--   We evaluate the term and check that the result value is #true.
runTestAssert
        :: Module RL
        -> Maybe Name -> Term RL -> IO ()

runTestAssert mm mnTest mTest
 = do
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        let state
                = Eval.State
                { Eval.stateConfig      = Eval.configDefault
                , Eval.stateModule      = mm }

        vResult <- Eval.evalTerm state rlNone (TermEnv []) mTest
        case vResult of
         [VTrue]  -> putStrLn "ok"
         [VFalse] -> putStrLn "failed"
         _        -> putStrLn "invalid"

