
module Salt.Llvm.Syntax.Metadata
        ( Metadata      (..)
        , tbaaNode
        , tbaaRoot
        , MDecl         (..)
        , MRef          (..)
        , MDString      (..)
        , MDNode        (..)
        , MDNodeOp      (..)
        , rval )
where
import Salt.Llvm.Syntax.Type
import Data.Text        (Text)


-- Metadata types -------------------------------------------------------------
-- | Different types of metadata used in LLVM IR
--      e.g. 'debug', 'tbaa', 'range', etc.
data Metadata
        -- Metadata used for type-based alias analysis.
        = Tbaa  MDNode

        -- Metadata for debugging, here as an example only.
        | Debug
        deriving (Eq, Show)


-- | Maps matadata references to metadata nodes
--      e.g. !2 = !{ metadata "id", !0, !i11}
data MDecl
        = MDecl MRef Metadata
        deriving (Eq, Show)


-- | Reference to a metadata node.
data MRef
        = MRef  Int
        deriving (Eq, Show)


rval :: MDecl -> Metadata
rval (MDecl _ m) = m


-- Metadata internal-----------------------------------------------------------
-- | Primitive types of LLVM metadata
data MDString
        = MDString Text
        deriving (Eq, Show)


data MDNode
        = MDNode [MDNodeOp]
        deriving (Eq, Show)


-- Operands to metadata nodes
data MDNodeOp
        = OpNull
        | OpMDString  MDString
        | OpMDNode    MDNode
        | OpMDRef     MRef
        | OpBool      Bool
        | OpType      Type
        deriving (Eq, Show)


-- TBAA metadata --------------------------------------------------------------
-- | Construct a single tbaa node
tbaaNode
      :: Text           -- ^ A unique identifier for the node
      -> MRef           -- ^ The parent node
      -> Bool           -- ^ Whether this node represents a const region
      -> Metadata

tbaaNode n pr c
 = Tbaa $ MDNode
        [ OpMDString (MDString n)
        , OpMDRef     pr
        , OpBool      c ]


-- | Construct the tbaa root node.
tbaaRoot :: Text -> Metadata
tbaaRoot n
 = Tbaa $ MDNode
        [ OpMDString (MDString n) ]

