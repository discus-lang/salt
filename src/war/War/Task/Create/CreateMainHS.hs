
module War.Task.Create.CreateMainHS where
import War.Task.Create.Way
import War.Task.Job                     ()
import War.Driver.Base
import qualified War.Task.Job.CompileHS as CompileHS
import qualified War.Task.Job.RunExe    as RunExe


-- | Compile and run Main.hs files.
--   When we run the exectuable, pass it out build dir as the first argument.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way _allFiles filePath
 | takeFileName filePath == "Main.hs"
 = let
        sourceDir       = takeDirectory  filePath
        buildDir        = sourceDir </> "war-" ++ wayName way
        testName        = filePath

        mainBin         = buildDir </> "Main.bin"
        mainCompStdout  = buildDir </> "Main.compile.stdout"
        mainCompStderr  = buildDir </> "Main.compile.stderr"
        mainRunStdout   = buildDir </> "Main.run.stdout"
        mainRunStderr   = buildDir </> "Main.run.stderr"

        compile         = jobOfSpec (JobId testName (wayName way))
                        $ CompileHS.Spec
                                filePath []
                                buildDir mainCompStdout mainCompStderr
                                mainBin

        run             = jobOfSpec (JobId testName (wayName way))
                        $ RunExe.Spec
                                filePath
                                mainBin [buildDir]
                                mainRunStdout mainRunStderr
                                True

   in   Just $ Chain [compile, run]

 | otherwise    = Nothing
