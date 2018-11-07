
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
         _ -> error "no mode specified"


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

runCheck :: FilePath -> Module IW.Location -> IO ()
runCheck filePath mm
 = do   let a = IW.Location 0 0
        (_mm', errs)    <- Check.checkModule a mm

        case errs of
         [] -> return ()
         _  -> do
                mapM_ (printError filePath) errs
                System.exitFailure

printError filePath err
 = do   let (IW.Location nLine nCol) = Error.errorAnnot err
        putStrLn
         $ P.renderIndent $ vcat
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
        runCheck filePath mm

        let tests = [ d | DTest d <- moduleDecls mm
                        , declTestName d == Just (Name name) ]

        case tests of
         []     -> error $ "mainTest: no test named: " ++ show name
         _      -> mapM_ (runTest mm) tests


mainTests :: FilePath -> IO ()
mainTests filePath
 = do   mm      <- runParse filePath
        runCheck filePath mm

        let tests = [ d | DTest d <- moduleDecls mm ]
        mapM_ (runTest mm) tests


runTest :: Module IW.Location -> DeclTest IW.Location -> IO ()
runTest mm tt
 = case tt of
        DeclTestKind    _ n t   -> runTestKind   mm n t
        DeclTestType    _ n m   -> runTestType   mm n m
        DeclTestEval    _ n m   -> runTestEval   mm n m
        DeclTestAssert  _ n m   -> runTestAssert mm n m


-- | Run a kind test.
--   We kind-check a type and print the result kind.
runTestKind
        :: Module IW.Location
        -> Maybe Name -> Type IW.Location -> IO ()
runTestKind _mm mnTest tTest
 = do
        let a   = IW.Location 0 0
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        (_t, kResult) <- Check.checkType a [] Check.contextEmpty tTest
        putStrLn $ P.renderIndent $ P.ppr () kResult


-- | Run a type test.
--   We type-check a term and print the result type.
runTestType
        :: Module IW.Location
        -> Maybe Name -> Term IW.Location -> IO ()
runTestType _mm mnTest mTest
 = do
        let a   = IW.Location 0 0
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        (_m, tResult) <- Check.checkTerm1 a [] Check.contextEmpty mTest Check.Synth
        putStrLn $ P.renderIndent $ P.ppr () tResult


-- | Run a eval test.
--   We evaluate the term and print the result value.
runTestEval
        :: Module IW.Location
        -> Maybe Name -> Term IW.Location -> IO ()
runTestEval _mm mnTest mTest
 = do
        let a   = IW.Location 0 0
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        vResult <- Eval.evalTerm a (Env []) mTest
        putStrLn $ P.renderIndent $ P.ppr () vResult


-- | Run an assert test.
--   We evaluate the term and check that the result value is #true.
runTestAssert
        :: Module IW.Location
        -> Maybe Name -> Term IW.Location -> IO ()
runTestAssert _mm mnTest mTest
 = do
        let a   = IW.Location 0 0
        putStr  $ "* "
                ++ (case mnTest of
                        Nothing -> ""
                        Just (Name tx) -> T.unpack tx % ": ")
        System.hFlush System.stdout

        vResult <- Eval.evalTerm a (Env []) mTest
        case vResult of
         [VTrue]        -> putStrLn "ok"
         [VFalse]       -> putStrLn "failed"
         _              -> putStrLn "invalid"

