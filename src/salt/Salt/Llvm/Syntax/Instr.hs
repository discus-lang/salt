
module Salt.Llvm.Syntax.Instr
        ( -- * Blocks
          Block         (..)
        , Label         (..)

        -- * Instructions
        , Instr         (..)
        , branchTargetsOfInstr
        , defVarOfInstr
        , defVarsOfBlock)
where
import Salt.Llvm.Syntax.Exp
import Salt.Llvm.Syntax.Prim
import Salt.Llvm.Syntax.Attr
import Salt.Llvm.Syntax.Metadata
import Salt.Llvm.Syntax.Type
import Data.Maybe
import Data.Sequence            (Seq)
import Data.Set                 (Set)
import qualified Data.Set       as Set
import qualified Data.Foldable  as Seq
import Data.Text                (Text)


-- | Block labels.
data Label
        = Label Text
        deriving (Eq, Ord, Show)


-- | A block of LLVM code with an entry label.
data Block
        = Block
        { -- | The code label for this block
          blockLabel    :: Label

          -- | A list of Instructions for ths block.
          --   This list must end with a control flow terminator instruction.
        , blockInstrs   :: Seq Instr
        }


-- | Instructions
data Instr
        -- Meta -------------------------------------------
        -- | Wrap an instruction in meta-data.
        = IMeta         Instr [MDecl]

        -- Phi nodes --------------------------------------
        | IPhi          Var    [(Exp, Label)]

        -- Terminator Instructions ------------------------
        -- | Return a result.
        | IReturn       (Maybe Exp)

        -- | Unconditional branch to the target label.
        | IBranch       Label

        -- | Conditional branch.
        | IBranchIf     Exp     Label  Label

        -- | Mutliway branch.
        --   If scruitniee matches one of the literals in the list then jump
        --   to the corresponding label, otherwise jump to the default.
        | ISwitch       Exp     Label  [(Lit, Label)]

        -- | Informs the optimizer that instructions after this point are unreachable.
        | IUnreachable

        -- Binary Operations ------------------------------
        | IOp           Var     Op      Exp     Exp

        -- Conversion Operations --------------------------
        -- | Cast the variable from to the to type. This is an abstraction of three
        --   cast operators in Llvm, inttoptr, prttoint and bitcast.
        | IConv         Var     Conv    Exp

        -- | Get element pointer.
        | IGet          Var     Exp     [Exp]

        -- Memory Access and Addressing -------------------
        -- | Allocate space for a value of the given type on the stack.
        | IAlloca       Var     Type

        -- | Load a value from memory.
        | ILoad         Var     Exp

        -- | Store a value to memory.
        --   First expression gives the destination pointer.
        | IStore        Exp     Exp

        -- Other Operations -------------------------------
        -- | Comparisons
        | ICmp          Var    Cond   Exp    Exp

        -- | Call a function.
        --   Only NoReturn, NoUnwind and ReadNone attributes are valid.
        | ICall         (Maybe Var) CallType (Maybe CallConv)
                        Type Name [Exp] [FuncAttr]
        deriving (Show, Eq)


-- | If this instruction can branch to a label then return the possible targets.
branchTargetsOfInstr :: Instr -> Maybe (Set Label)
branchTargetsOfInstr instr
 = case instr of
        IBranch l
         -> Just $ Set.singleton l

        IBranchIf _ l1 l2
         -> Just $ Set.fromList [l1, l2]

        ISwitch _ lDef ls
         -> Just $ Set.fromList (lDef : map snd ls)

        _ -> Nothing


-- | Get the LLVM variable that this instruction assigns to,
--   or `Nothing` if there isn't one.
defVarOfInstr :: Instr -> Maybe Var
defVarOfInstr instr
 = case instr of
        IMeta i _       -> defVarOfInstr i
        IPhi var _      -> Just var
        IReturn{}       -> Nothing
        IBranch{}       -> Nothing
        IBranchIf{}     -> Nothing
        ISwitch{}       -> Nothing
        IUnreachable{}  -> Nothing
        IOp var _ _ _   -> Just var
        IConv var _ _   -> Just var
        IGet  var _ _   -> Just var
        IAlloca var _   -> Just var
        ILoad var _     -> Just var
        IStore{}        -> Nothing
        ICmp var _ _ _  -> Just var
        ICall mvar _ _ _ _ _ _ -> mvar


-- | Get the set of LLVM variables that this block defines.
defVarsOfBlock :: Block -> Set Var
defVarsOfBlock (Block _ instrs)
 = Set.fromList $ mapMaybe defVarOfInstr $ Seq.toList instrs

