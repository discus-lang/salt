
module Salt.Llvm.Write.Function where
import Salt.Llvm.Syntax.Function
import Salt.Llvm.Syntax.Type
import Salt.Llvm.Write.Type      ()
import Salt.Llvm.Write.Attr      ()
import Salt.Llvm.Write.Instr     ()
import Salt.Llvm.Write.Base
import Data.Text                (Text)
import qualified Data.Text      as T


instance Write Config Function where
 write o (Function decl nsParam attrs sec blocks)
  = do
        text o "define "
        writeFunctionDeclWithNames o decl (Just $ map T.pack nsParam)

        space o
        punc  o " " attrs

        space o
        (case sec of
          SectionAuto           -> return ()
          SectionSpecific s
           -> do text o "section "; dquotes o (text o s))

        space o; text  o "{"; line o
        vwrite o blocks
        line  o; text  o "}"; line  o


writeFunctionDeclWithNames :: Config -> FunctionDecl -> Maybe [Text] -> IO ()
writeFunctionDeclWithNames o
        (FunctionDecl name linkage callConv tReturn varg params align mGcStrat)
        mnsParams
 = do
        write o linkage;  space o
        write o callConv; space o
        write o tReturn
        text  o " @"; text o name

        parens o $ do
            (case mnsParams of
              Nothing
               -> punc' o ", "
                    [ do write o t
                         (case attrs of
                            []      -> return ()
                            as      -> do space o; punc o " " as)
                    | Param t attrs <- params ]

              Just nsParams
               -> punc' o ", "
                    [ do write o t
                         (case attrs of
                            []      -> return ()
                            as      -> do space o; punc o " " as)
                         text o " %"; text o nParam
                    | Param t attrs <- params
                    | nParam <- nsParams ])

            (case varg of
              VarArgs | null params -> text o "..."
                      | otherwise   -> text o ", ..."
              _                     -> return ())

        (case align of
                AlignNone       -> return ()
                AlignBytes b    -> do text o " align "; write o b)

        (case mGcStrat of
                Nothing         -> return ()
                Just sStrat     -> do text o " gc "; dquotes o (text o sStrat))

