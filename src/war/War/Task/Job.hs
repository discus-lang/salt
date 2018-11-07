{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS -fno-warn-orphans #-}
module War.Task.Job where
import War.Driver.Base
import qualified War.Task.Job.Diff       as Diff
import qualified War.Task.Job.CompileHS  as CompileHS
import qualified War.Task.Job.RunSalt    as RunSalt
import qualified War.Task.Job.RunExe     as RunExe
import qualified War.Task.Job.Shell      as Shell


instance Spec Diff.Spec Diff.Result where
 specActionName _       = "diff"
 buildFromSpec          = Diff.build
 productOfResult _ result
  = case result of
        Diff.ResultSame
         -> ProductStatus (ppr result) True

        Diff.ResultDiff ref out' diff
         -> ProductDiff ref out' diff


instance Spec CompileHS.Spec CompileHS.Result where
 specActionName _       = "compile"
 buildFromSpec          = CompileHS.build
 productOfResult _ result
        = ProductStatus (ppr result) (CompileHS.resultSuccess result)


instance Spec RunSalt.Spec RunSalt.Result where
 specActionName _       = "run"
 buildFromSpec          = RunSalt.build
 productOfResult _ result
        = ProductStatus (ppr result) (RunSalt.resultSuccess result)


instance Spec RunExe.Spec RunExe.Result where
 specActionName _       = "run"
 buildFromSpec          = RunExe.build
 productOfResult _ result
        = ProductStatus (ppr result) (RunExe.resultSuccess result)


instance Spec Shell.Spec Shell.Result where
 specActionName _       = "shell"
 buildFromSpec          = Shell.build
 productOfResult _ result
        = ProductStatus (ppr result) (Shell.resultSuccess result)

