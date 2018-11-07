
module War.Task.Create.CreateSalt where
import War.Task.Create.Way
import War.Task.Job                     ()
import War.Driver.Base
import qualified War.Task.Job.RunSalt   as RunSalt
import qualified War.Task.Job.Diff      as Diff
import qualified Data.Set               as Set


-- | Run .salt files with the salt interpreter.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | takeFileName filePath == "Test.salt"
 = let
        fileName        = takeFileName filePath
        sourceDir       = takeDirectory  filePath
        buildDir        = sourceDir </> "war-" ++ wayName way
        testName        = filePath

        saltStdout      = buildDir  </> replaceExtension fileName ".salt.stdout"
        saltStderr      = buildDir  </> replaceExtension fileName ".salt.stderr"
        stdoutCheck     = sourceDir </> "Test.salt.stdout.check"
        stdoutDiff      = buildDir  </> "Test.salt.stdout.diff"
        errorCheck      = sourceDir </> "Test.salt.error.check"
        shouldSucceed   = not $ Set.member errorCheck allFiles

        jobRun          = jobOfSpec (JobId testName (wayName way))
                        $ RunSalt.Spec filePath buildDir
                                saltStdout saltStderr
                                shouldSucceed

        jobDiffOut      = jobOfSpec (JobId testName (wayName way))
                        $ Diff.Spec stdoutCheck saltStdout stdoutDiff

        jobDiffErr      = jobOfSpec (JobId testName (wayName way))
                        $ Diff.Spec errorCheck  saltStdout stdoutDiff

   in   Just $ Chain
                $ [jobRun]
                ++ (if Set.member stdoutCheck allFiles then [jobDiffOut] else [])
                ++ (if Set.member errorCheck  allFiles then [jobDiffErr] else [])

 | otherwise    = Nothing

