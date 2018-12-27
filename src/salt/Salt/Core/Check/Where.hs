
module Salt.Core.Check.Where where
import Salt.Core.Exp


-- | Tracks where we are a source program during type checking,
--   to help error reporting.
data Where a
        -- type decls -------------------------------------
        = WhereTypeDecl
        { whereAnnot            :: a
        , whereDeclName         :: Name }

        -- term decls -------------------------------------
        | WhereTermDecl
        { whereAnnot            :: a
        , whereDeclName         :: Name }

        -- test decls -------------------------------------
        | WhereTestDecl
        { whereAnnot            :: a
        , whereTestName         :: Maybe Name }

        -- terms ------------------------------------------
        | WhereAppPrim
        { whereAnnot            :: a
        , wherePrimName         :: Name
        , wherePrimType         :: Type a }

        | WhereRecordField
        { whereAnnot            :: a
        , whereLabel            :: Name
        , whereTypeExpected     :: Maybe [Type a] }
        deriving Show

