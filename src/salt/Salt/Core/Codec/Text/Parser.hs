
module Salt.Core.Codec.Text.Parser where
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Term
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Control.Monad
import Text.Parsec                              ((<?>))
import qualified Text.Parsec                    as P


---------------------------------------------------------------------------------------------------
-- | Run a parser on a list of located tokens.
parse   :: Parser a -> FilePath -> [At Token]
        -> Either P.ParseError (a, [At Token])
parse p f ts
 = P.parse
        (do result <- p
            rest   <- P.getInput
            return (result, rest))
        f ts

---------------------------------------------------------------------------------------------------
-- | Parser for a module.
pModule :: Parser (Module Location)
pModule
 = do   decls   <- P.many pDecl
        pTok KEnd
        return  $ Module decls


---------------------------------------------------------------------------------------------------
-- | Parser for a top-level declaration.
pDecl :: Parser (Decl Location)
pDecl
 = P.choice
 [ do   -- 'test' Name? 'print'  Term
        -- 'test' Name? 'assert' Term
        loc <- getLocation
        pTok KTest

        P.choice
         [ do
                nMode   <- P.choice
                        [ do    n <- pVar
                                (guard $ elem n ["kind", "type", "eval", "assert"])
                                return n

                        , do    -- 'type' is both a test specifier and a keyword,
                                -- so we need to match for it explicitly.
                                pTok KType
                                return "type" ]
                        <?> "test mode specifier"

                mName <- P.optionMaybe $ do pTok KDot; pVar

                P.choice
                 [ do   guard $ nMode == "kind"
                        tType   <- pType
                        return  $ DTest $ DeclTestKind
                                { declAnnot     = loc
                                , declTestName  = mName
                                , declTestType  = tType }

                 , do   guard $ nMode == "type"
                        mTerm   <- pTerm
                        return  $ DTest $ DeclTestType
                                { declAnnot     = loc
                                , declTestName  = mName
                                , declTestTerm  = mTerm }

                 , do   guard $ nMode == "eval"
                        mBody   <- pTerm
                        return  $ DTest $ DeclTestEval
                                { declAnnot     = loc
                                , declTestName  = mName
                                , declTestBody  = mBody }

                 , do   guard $ nMode == "assert"
                        mBody   <- pTerm
                        return  $ DTest $ DeclTestAssert
                                { declAnnot     = loc
                                , declTestName  = mName
                                , declTestBody  = mBody }

                 ]
         ]
         <?> "a test declaration"
 ]
 <?> "a declaration"

