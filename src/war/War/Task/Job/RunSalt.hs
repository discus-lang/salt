
module War.Task.Job.RunSalt
        ( Spec    (..)
        , Result  (..)
        , resultSuccess
        , build)
where
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox.Data.Physical
import BuildBox.Pretty
import BuildBox
import System.Directory
import System.FilePath


-- | Feed a .salt file into the salt interpreter.
data Spec
        = Spec
        { -- | Root source file of the program (the 'Test.salt')
          specFile               :: FilePath

          -- | Scratch dir to do the build in.
        , specScratchDir         :: String

          -- | Put what it says to stdout here.
        , specCompileStdout      :: FilePath

          -- | Put what it says to stderr here.
        , specCompileStderr      :: FilePath

          -- | Whether the running the command should succeed
        , specShouldSucceed     :: Bool }
        deriving Show


-- | Possible results of this job.
data Result
        = ResultSuccess Seconds
        | ResultUnexpectedFailure
        | ResultUnexpectedSuccess


-- | Check if the result has been successful.
resultSuccess :: Result -> Bool
resultSuccess result
 = case result of
        ResultSuccess{} -> True
        _               -> False


instance Pretty Result where
 ppr result
  = case result of
        ResultSuccess seconds
         -> string "success" %% parens (ppr seconds)

        ResultUnexpectedFailure
         -> string "failed"

        ResultUnexpectedSuccess
         -> string "unexpected"


-- | Feed a .salt file into the salt interpreter.
build :: Spec -> Build Result
build (Spec srcSalt buildDir testRunStdout testRunStderr bShouldSucceed)
 = do
        let saltExe = "bin/salt" <.> exe

        needs srcSalt
        needs saltExe

        -- ensure the output directory exists
        ensureDir buildDir

        saltExe' <- io $ canonicalizePath saltExe

        (time, (code, strOut, strErr))
          <- timeBuild
          $  systemTee False
                (saltExe' ++ " " ++ srcSalt)
                ""

        atomicWriteFile testRunStdout strOut
        atomicWriteFile testRunStderr strErr

        case code of
         ExitSuccess
          | bShouldSucceed -> return $ ResultSuccess time
          | otherwise      -> return $ ResultUnexpectedSuccess

         ExitFailure _
          | bShouldSucceed -> return $ ResultUnexpectedFailure
          | otherwise      -> return $ ResultSuccess time


