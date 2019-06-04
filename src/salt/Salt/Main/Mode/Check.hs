
module Salt.Main.Mode.Check where
import Salt.Main.Mode.Parse
import Salt.Core.Exp
import Salt.Data.Location
import Salt.Data.Pretty
import qualified Salt.Core.Check                as Check
import qualified Salt.Core.Check.Error          as Error
import qualified Salt.Core.Check.Where          as Where
import qualified Salt.Data.Pretty               as P

import qualified System.Exit                    as System


-- | Check a source module and print any errors to stdout.
mainCheck :: FilePath -> IO ()
mainCheck filePath
 = do   mm      <- runParse filePath
        runCheck filePath mm
        return ()


-- | Check a source module and return the resulting top-level context.
runCheck :: FilePath -> Module RL -> IO (Module RL, Check.Context RL)
runCheck filePath mm
 = do   Check.checkModule rlNone mm
         >>= \case
                Right (mm', ctx)
                 -> return (mm', ctx)
                Left errs
                 -> do  mapM_ (printError filePath) errs
                        System.exitFailure

printError filePath err
 = do   let (Range (Location nLine nCol) _)
                = Error.errorAnnot err
        putStrLn
         $ P.render $ P.vcat
         $      [ P.padL 6 $ P.string filePath
                        % P.text ":" % P.string (show (nLine + 1))
                        % P.text ":" % P.string (show (nCol + 1))
                , P.indent 2 $ ppr () err ]

         ++ [ P.empty ]
         ++ [ let (Range (Location nLine' nCol') _)
                    = Where.whereAnnot wh
              in  P.indent 2
                        $  P.padL 6
                                ( P.string (show (nLine' + 1))
                                % P.text ":" % (P.string (show (nCol' + 1))))
                        %% P.ppr () wh
                | wh <- Error.errorWhere err ]

         ++ [P.empty]

