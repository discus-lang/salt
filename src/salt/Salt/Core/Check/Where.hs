
module Salt.Core.Check.Where where
import Salt.Core.Exp


-- | Tracks where we are a source program during type checking,
--   to help error reporting.
data Where a
        = WhereTestEval
        { whereAnnot            :: a
        , whereTestName         :: Maybe Name }

        | WhereTestAssert
        { whereAnnot            :: a
        , whereTestName         :: Maybe Name }

        | WhereTestScenario
        { whereAnnot            :: a
        , whereTestName         :: Maybe Name }

        | WhereAppPrim
        { whereAnnot            :: a
        , wherePrimName         :: Name
        , wherePrimType         :: Type a }

        | WhereRecordField
        { whereAnnot            :: a
        , whereLabel            :: Name
        , whereTypeExpected     :: Maybe (Type a) }
        deriving Show

