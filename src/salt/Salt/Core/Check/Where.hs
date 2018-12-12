
module Salt.Core.Check.Where where
import Salt.Core.Exp


-- | Tracks where we are a source program during type checking,
--   to help error reporting.
data Where a
        = WhereTestType
        { whereAnnot            :: a
        , whereTestName         :: Maybe Name }

        | WhereTestEval
        { whereAnnot            :: a
        , whereTestName         :: Maybe Name }

        | WhereTestExec
        { whereAnnot            :: a
        , whereTestName         :: Maybe Name }

        | WhereTestAssert
        { whereAnnot            :: a
        , whereTestName         :: Maybe Name }

        | WhereTermDecl
        { whereAnnot            :: a
        , whereDeclName         :: Name }

        | WhereAppPrim
        { whereAnnot            :: a
        , wherePrimName         :: Name
        , wherePrimType         :: Type a }

        | WhereRecordField
        { whereAnnot            :: a
        , whereLabel            :: Name
        , whereTypeExpected     :: Maybe [Type a] }
        deriving Show

