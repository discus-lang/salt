
module Salt.Main.Mode.Emit where
import Salt.Main.Mode.Check
import Salt.Main.Mode.Parse
import Salt.Core.Exp
import Salt.Data.Location
import qualified Salt.Core.Eval                 as Eval
import qualified Salt.Core.Check                as Check
import qualified Salt.Core.Codec.Text.Pretty    as P


-- | Run all the emit declarations in the given source file.
--   Emit declarations are the main hook when using salt as a compiler.
--   We evaluate the provided term to a compiled bundle and print the result.
mainEmits :: FilePath -> IO ()
mainEmits filePath
 = do   mm         <- runParse filePath
        (mm', ctx) <- runCheck filePath mm

        let emits = [ d | DEmit d <- moduleDecls mm' ]
        mapM_ (runEmit ctx mm') emits


-- | Evalaute a code bundle and emit the result
mainEmit :: FilePath -> Text -> IO ()
mainEmit filePath name
 = do   mm         <- runParse filePath
        (mm', ctx) <- runCheck filePath mm

        let emits = [ d | DEmit d <- moduleDecls mm'
                        , declEmitName d == Just (Name name) ]

        case emits of
         []     -> error $ "mainEmit: no emit named: " ++ show name
         _      -> mapM_ (runEmit ctx mm') emits


-- | Run a single emit declareation.
runEmit :: Check.Context RL -> Module RL -> DeclEmit RL -> IO ()
runEmit _ctx mm (DeclEmit _ _ mEmit)
 = do
        -- Initialize the machine state.
        state   <- Eval.newState Eval.configDefault mm

        -- Evaluate the term in an empty environment and print the result.
        vsResult <- Eval.evalTerm state rlNone (TermEnv []) mEmit
        case vsResult of
                [VBundle bundle]
                   -> putStrLn $ P.render $ P.ppBundleGuts bundle

                [] -> return ()
                _  -> putStrLn $ P.render $ P.ppr () vsResult
