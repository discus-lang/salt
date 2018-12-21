
module Main where
import Salt.Main.Config
import Salt.Core.Exp
import Salt.Data.Pretty
import Data.Function
import qualified Salt.Core.Eval                 as Eval
import qualified Salt.Core.Check                as Check
import qualified Salt.Core.Check.Error          as Error
import qualified Salt.Core.Check.Where          as Where
import qualified Salt.Core.Codec.Text.Lexer     as Lexer
import qualified Salt.Core.Codec.Text.Token     as Token
import qualified Salt.Core.Codec.Text.Parser    as Parser
import qualified Salt.Data.Pretty               as P
import qualified System.Environment             as System
import qualified System.Exit                    as System
import qualified System.IO                      as System
import qualified Text.Lexer.Inchworm.Char       as IW
import qualified Text.Show.Pretty               as Show
import qualified Data.Text                      as T
import qualified Data.Map.Strict                as Map


-- Main -------------------------------------------------------------------------------------------
main
 = do   args    <- System.getArgs
        config  <- parseArgs args configDefault
        case configMode config of
         Just (ModeLex   filePath)      -> mainLex   filePath
         Just (ModeParse filePath)      -> mainParse filePath
         Just (ModeCheck filePath)      -> mainCheck filePath
         Just (ModeTest  filePath)      -> mainTests filePath
         Just (ModeTest1 filePath name) -> mainTest  filePath name
         _ -> do  putStr usage
                  System.exitFailure


-- Lex --------------------------------------------------------------------------------------------
mainLex filePath
 = do   toks       <- runLex filePath
        putStr  $ unlines $ map show toks

runLex filePath
 = do   source  <- readFile filePath

        (toks, loc, strRest)
         <- IW.scanStringIO source (Lexer.scanner filePath)

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


runParse  :: FilePath -> IO (Module Lexer.Location)
runParse filePath
 = do   toks       <- runLex filePath
        let result
                = Parser.parse Parser.pModule filePath
                $ toks ++ [Token.At (IW.Location 0 0) Token.KEnd]

        case result of
         Left  err
          -> do print err
                System.exitFailure

         Right (xx, [])
          -> do return xx

         Right (_, xsRest)
          -> do putStrLn "parse error at end of input"
                print xsRest
                System.exitFailure


-- Check ------------------------------------------------------------------------------------------
mainCheck :: FilePath -> IO ()
mainCheck filePath
 = do   mm      <- runParse filePath
        runCheck filePath mm
        return ()

runCheck :: FilePath -> Module IW.Location
         -> IO (Check.Context IW.Location)
runCheck filePath mm
 = do   let a = IW.Location 0 0
        (ctx, _mm', errs) <- Check.checkModule a mm
        case errs of
         [] -> return ctx
         _  -> do
                mapM_ (printError filePath) errs
                System.exitFailure

printError filePath err
 = do   let (IW.Location nLine nCol) = Error.errorAnnot err
        putStrLn
         $ P.render $ vcat
         $      [ P.padL 6 $ P.string filePath
                        % text ":" % P.string (show nLine)
                        % text ":" % P.string (show nCol)
                , P.indent 2 $ ppr () err ]

         ++ [ empty]
         ++ [ let (IW.Location nLine' nCol') = Where.whereAnnot wh
                  in  P.indent 2
                        $  P.padL 6 (P.string (show nLine') % text ":" % P.string (show nCol'))
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


runTest :: Check.Context IW.Location  -> Module IW.Location
        -> DeclTest IW.Location -> IO ()
runTest ctx mm tt
 = case tt of
        DeclTestKind    _ n t   -> runTestKind   ctx mm n t
        DeclTestType    _ n m   -> runTestType   ctx mm n m
        DeclTestEval    _ n m   -> runTestEval   mm n m
        DeclTestExec    _ n m   -> runTestExec   mm n m
        DeclTestAssert  _ n m   -> runTestAssert mm n m


-- TestKind ---------------------------------------------------------------------------------------
-- | Run a kind test.
--   We kind-check a type and print the result kind.
runTestKind
        :: Check.Context IW.Location -> Module IW.Location
        -> Maybe Name -> Type IW.Location -> IO ()
runTestKind ctx _mm mnTest tTest
 = do
        let a   = IW.Location 0 0
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        (_t, kResult) <- Check.checkType a [] ctx tTest
        putStrLn $ P.render $ P.ppr () kResult


-- TestType ---------------------------------------------------------------------------------------
-- | Run a type test.
--   We type-check a term and print the result type.
runTestType
        :: Check.Context IW.Location -> Module IW.Location
        -> Maybe Name -> Term IW.Location -> IO ()
runTestType ctx _mm mnTest mTest
 = do
        let a   = IW.Location 0 0
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        (_m, tsResult, _esResult)
         <- Check.checkTerm a [] ctx Check.Synth mTest
        case tsResult of
         [t]    -> putStrLn $ P.render $ P.ppr () t
         _      -> putStrLn $ P.render $ P.ppr () tsResult


-- TestEval ---------------------------------------------------------------------------------------
-- | Run a eval test.
--   We evaluate the term and print the result value.
runTestEval
        :: Module IW.Location
        -> Maybe Name -> Term IW.Location -> IO ()
runTestEval mm mnTest mTest
 = do
        let a   = IW.Location 0 0
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        let declTerms
                = Map.fromList
                [ (n, d) | DTerm d@(DeclTerm _ n _ _ _) <- moduleDecls mm ]

        let state
                = Eval.State
                { Eval.stateConfig      = Eval.configDefault
                , Eval.stateDeclTerms   = declTerms }

        vResult <- Eval.evalTerm state a (Env []) mTest
        putStrLn $ P.render $ P.ppr () vResult


-- TestExec ---------------------------------------------------------------------------------------
-- | Run a exec test.
--   We evaluate the term to a suspension then run it.
runTestExec
        :: Module IW.Location
        -> Maybe Name -> Term IW.Location -> IO ()
runTestExec mm mnTest mTest
 = do
        let a   = IW.Location 0 0
        (case mnTest of
                Nothing -> return ()
                Just (Name tx) -> do
                        putStr $ T.unpack tx % ": "
                        System.hFlush System.stdout)

        let declTerms
                = Map.fromList
                [ (n, d) | DTerm d@(DeclTerm _ n _ _ _) <- moduleDecls mm ]

        let state
                = Eval.State
                { Eval.stateConfig      = Eval.configDefault
                , Eval.stateDeclTerms   = declTerms }

        vsSusp <- Eval.evalTerm state a (Env []) mTest

        case vsSusp of
                [VClosure (Closure (Env []) (MPTerms []) mBody)]
                 -> do  vsResult <- Eval.evalTerm state a (Env []) mBody
                        case vsResult of
                         [] -> return ()
                         _  -> putStrLn $ P.render $ P.ppr () vsResult

                _ -> error $ "runTestEval: term did not produce a suspension"
                           ++ show vsSusp


-- TestAssert -------------------------------------------------------------------------------------
-- | Run an assert test.
--   We evaluate the term and check that the result value is #true.
runTestAssert
        :: Module IW.Location
        -> Maybe Name -> Term IW.Location -> IO ()
runTestAssert mm mnTest mTest
 = do
        let a   = IW.Location 0 0
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        let declTerms
                = Map.fromList
                [ (n, d) | DTerm d@(DeclTerm _ n _ _ _) <- moduleDecls mm ]

        let state
                = Eval.State
                { Eval.stateConfig      = Eval.configDefault
                , Eval.stateDeclTerms   = declTerms }

        vResult <- Eval.evalTerm state a (Env []) mTest
        case vResult of
         [VTrue]        -> putStrLn "ok"
         [VFalse]       -> putStrLn "failed"
         _              -> putStrLn "invalid"

