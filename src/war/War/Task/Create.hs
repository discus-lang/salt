
-- | Creation of test jobs.
--   We decide what to do based on what files are in a directory.
module War.Task.Create
        ( Way   (..)
        , create)
where
import War.Task.Create.Way
import War.Driver.Base
import qualified War.Task.Create.CreateMainSH   as CreateMainSH
import qualified War.Task.Create.CreateMainHS   as CreateMainHS
import qualified War.Task.Create.CreateSalt     as CreateSalt


-- | Create job chains based on this file.
create  :: Way                  -- ^ Create tests for this way.
        -> Set FilePath         -- ^ All files in the test directory.
        -> FilePath             -- ^ Create test chains based on this file.
        -> [Chain]

create way allFiles filePath
 = let  creations
         = [ CreateMainSH.create
           , CreateMainHS.create
           , CreateSalt.create ]

   in   catMaybes [ creat way allFiles filePath
                  | creat <- creations]

