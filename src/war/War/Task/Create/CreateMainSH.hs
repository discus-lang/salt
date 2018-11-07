
module War.Task.Create.CreateMainSH where
import War.Task.Create.Way
import War.Task.Job                     ()
import War.Driver.Base
import qualified War.Task.Job.Shell     as Shell
import qualified War.Task.Job.Diff      as Diff
import qualified Data.Set               as Set


-- | Run Main.sh files.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | takeFileName filePath == "Main.sh"
 = let
        sourceDir       = takeDirectory  filePath
        buildDir        = sourceDir </> "war-" ++ wayName way
        testName        = filePath


        mainShellStdout         = buildDir  </> "Main.shell.stdout"
        mainShellStderr         = buildDir  </> "Main.shell.stderr"
        mainShellStderrDiff     = buildDir  </> "Main.compile.stderr.diff"
        mainErrorCheck          = sourceDir </> "Main.error.check"
        shouldSucceed           = not $ Set.member mainErrorCheck allFiles

        shell           = jobOfSpec (JobId testName (wayName way))
                        $ Shell.Spec
                                filePath sourceDir buildDir
                                mainShellStdout mainShellStderr
                                shouldSucceed

        diffError       = jobOfSpec (JobId testName (wayName way))
                        $ Diff.Spec
                                mainErrorCheck
                                mainShellStderr mainShellStderrDiff

   in   Just $ Chain
         $  [shell]
         ++ (if shouldSucceed then [] else [diffError])

 | otherwise    = Nothing
